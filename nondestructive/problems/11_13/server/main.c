/* 11_13/test/main.c */
#include <chicken.h>
#include <stdio.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>

enum { 
    c_listen_queue_len = 16, 
    c_init_sess_arr_size = 32,
    c_inbufsize = 1024 
};

// @TODO: put all logic into scheme with non-trivial objects
// @TODO: make adequate interface

#define SESSION_FINISHED_STATE C_SCHEME_TRUE

struct session {
    int fd;
    unsigned int from_ip;
    unsigned short from_port;
    char buf[c_inbufsize];
    int buf_used;
    C_word state;
};

void session_send_str(struct session *sess, const char *str)
{
    // we send small strings => no need to wait for writefds, just send
    write(sess->fd, str, strlen(str));
}

struct session *make_session(int fd,
    unsigned int from_ip, unsigned short from_port)
{
    struct session *sess = malloc(sizeof(struct session));
    sess->fd = fd;
    sess->from_ip = ntohl(from_ip);
    sess->from_port = ntohs(from_port);
    sess->buf_used = 0;
    sess->state = C_fix(0);
    return sess;
}

void session_check_lf(struct session *sess, C_word logic)
{
    int pos = -1;
    char *line;
    C_word str_closure[2], *scp = str_closure;
    C_word str_closure_obj;
    C_word list_closure[C_SIZEOF_LIST(2)], *lcp = list_closure;
    C_word list_closure_obj;
    C_word result;
    int status;

    for (int i = 0; i < sess->buf_used; ++i) {
        if (sess->buf[i] == '\n') {
            pos = i;
            break;
        }
    }
    if (pos == -1) 
        return;

    line = malloc(pos + 1);
    memcpy(line, sess->buf, pos);
    line[pos] = '\0';
    sess->buf_used -= pos + 1;
    memmove(sess->buf, sess->buf + pos + 1, sess->buf_used);
    if (line[pos - 1] == '\r')
        line[--pos] = '\0';

    str_closure_obj = C_string2(&scp, line);
    list_closure_obj = C_list(&lcp, 2, sess->state, str_closure_obj);

    status = CHICKEN_apply(logic, list_closure_obj, &result);
    free(line);

    if (!status) {
        session_send_str(sess, "Server error occured\n");
        sess->state = SESSION_FINISHED_STATE;
        return;
    }

    sess->state = C_u_i_car(result);
    session_send_str(sess, C_c_string(C_u_i_cdr(result)));
}

int session_do_read(struct session *sess, C_word logic)
{
    int rc, bufp = sess->buf_used;
    rc = read(sess->fd, sess->buf + bufp, c_inbufsize - bufp);
    if (rc <= 0)
        return 0;

    sess->buf_used += rc;
    session_check_lf(sess, logic);
    if (sess->buf_used == c_inbufsize) {
        session_send_str(sess, "The input string is too long\n");
        return 0;
    }

    if (sess->state == SESSION_FINISHED_STATE)
        return 0;

    return 1;
}

struct server {
    int ls;
    struct session **sessions;
    int sessions_size;
};

int server_init(struct server *serv, int port)
{
    int sock, opt;
    struct sockaddr_in addr;

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock == -1) {
        perror("socket");
        return 0;
    }

    opt = 1;
    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(&opt));

    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(port);
    if (bind(sock, (struct sockaddr *)&addr, sizeof(addr)) == -1) {
        perror("bind");
        return 0;
    }

    listen(sock, c_listen_queue_len);
    serv->ls = sock;

    serv->sessions = calloc(c_init_sess_arr_size, sizeof(*serv->sessions));
    serv->sessions_size = c_init_sess_arr_size;

    return 1;
}

void server_accept_client(struct server *serv)
{
    int sd, i;
    struct sockaddr_in addr;
    socklen_t len = sizeof(addr);
    sd = accept(serv->ls, (struct sockaddr *)&addr, &len);
    if (sd == -1) {
        perror("accept");
        return;
    }

    if (sd >= serv->sessions_size) { // resize if needed
        int newsize = serv->sessions_size;
        while (newsize <= sd)
            newsize += c_init_sess_arr_size;
        serv->sessions = 
            realloc(serv->sessions, newsize * sizeof(*serv->sessions));
        for (i = serv->sessions_size; i < newsize; i++)
            serv->sessions[i] = NULL;
        serv->sessions_size = newsize;
    }

    serv->sessions[sd] = make_session(sd, addr.sin_addr.s_addr, addr.sin_port);
}

void server_close_session(struct server *serv, int sd)
{
    close(sd);
    free(serv->sessions[sd]);
    serv->sessions[sd] = NULL;
}

int chicken_init(C_word *logic_cb_out)
{
    void *obj;

    if (!CHICKEN_initialize(0, 0, 0, C_toplevel)) {
        fprintf(stderr, "Internal error: failed to init chicken context\n");
        return 0;
    }

    CHICKEN_run(NULL);

    obj = CHICKEN_global_lookup("step-session");
    if (!obj) {
        fprintf(stderr, "Internal error: failed fetch function\n");
        return 0;
    }

    *logic_cb_out = CHICKEN_global_ref(obj);
    return 1;
}

int main(int argc, char **argv)
{
    struct server serv;
    C_word logic;
    long port;
    char *endptr;

    if (argc != 2) {
        fprintf(stderr, "Args: <port>\n");
        return 1;
    }

    port = strtol(argv[1], &endptr, 10);
    if (!*argv[1] || *endptr) {
        fprintf(stderr, "Invalid port number\n");
        return 1;
    }
        
    if (!server_init(&serv, port)) {
        fprintf(stderr, "Failed to init server\n");
        return 2;
    }

    if (!chicken_init(&logic)) {
        fprintf(stderr, "Failed to init logic\n");
        return 2;
    }

    for (;;) {
        fd_set readfds;
        FD_ZERO(&readfds);
        FD_SET(serv.ls, &readfds);

        int maxfd = serv.ls;
        for (int i = 0; i < serv.sessions_size; i++) {
            if (serv.sessions[i]) {
                FD_SET(i, &readfds);
                if (i > maxfd)
                    maxfd = i;
            }
        }

        int sr = select(maxfd + 1, &readfds, NULL, NULL, NULL);
        if (sr == -1) {
            perror("select");
            return 2;
        }

        if (FD_ISSET(serv.ls, &readfds))
            server_accept_client(&serv);
        for (int i = 0; i < serv.sessions_size; i++) {
            if (serv.sessions[i] && FD_ISSET(i, &readfds)) {
                int ssr = session_do_read(serv.sessions[i], logic);
                if (!ssr)
                    server_close_session(&serv, i);
            }
        }
    }

    return 0;
}
