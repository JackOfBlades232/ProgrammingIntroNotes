
/* select.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>

#define LISTEN_QLEN 16
#define INIT_SESS_ARR_SIZE 32

enum { 
    port = 7654, 
    listen_queue_len = 16, 
    inbufsize = 1024 
};

enum fsm_state {
    fsm_start,
    fsm_name = fsm_start,
    fsm_age,
    fsm_town,
    fsm_band,
    fsm_finish,
    fsm_error
};

struct session {
    int fd;
    unsigned int from_ip;
    unsigned short from_port;
    char buf[inbufsize];
    int buf_used;
    enum fsm_state state;
    char *name, *town, *band;
    int age;
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
    sess->state = fsm_start;
    sess->name = NULL;
    sess->town = NULL;
    sess->band = NULL;
    sess->age = -1;
    session_send_str(sess, "What is thy name?\n");
    return sess;
}

void cleanup_session(struct session *sess)
{
    if (sess->name) 
        free(sess->name);
    if (sess->town) 
        free(sess->town);
    if (sess->band) 
        free(sess->band);
}

void session_handle_age(struct session *sess, const char *line)
{
    char *err;
    int age;
    age = strtol(line, &err, 10);
    if (!*line || *err || age < 0 || age > 150) {
        session_send_str(sess, "Such age is unbeknownst to me\n");
        session_send_str(sess, "How old art thou?\n");
    } else {
        sess->age = age;
        session_send_str(sess, "What town dost thou call thy birthplace?\n");
        sess->state = fsm_town;
    }
}

void session_fsm_step(struct session *sess, char *line)
{
    switch (sess->state) {
        case fsm_name:
            sess->name = line;
            session_send_str(sess, "How old art thou?\n");
            sess->state = fsm_age;
            break;
        case fsm_age:
            session_handle_age(sess, line);
            free(line); // since age is parsed to int
            break;
        case fsm_town:
            sess->town = line;
            session_send_str(sess, "What is thy favoured band?\n");
            sess->state = fsm_band;
            break;
        case fsm_band:
            sess->band = line;
            session_send_str(sess, "Safe travels\n");
            sess->state = fsm_finish;
            break;
        case fsm_finish:
        case fsm_error:
            free(line); // should not happen
    }
}

void session_check_lf(struct session *sess)
{
    int pos = -1;
    char *line;
    for (int i = 0; i < sess->buf_used; i++) {
        if (sess->buf[i] == '\n') {
            pos = i;
            break;
        }
    }
    if (pos == -1) 
        return;

    line = malloc(pos+1);
    memcpy(line, sess->buf, pos);
    line[pos] = '\0';
    sess->buf_used -= pos+1;
    memmove(sess->buf, sess->buf+pos+1, sess->buf_used);
    if (line[pos-1] == '\r')
        line[pos-1] = '\0';
    session_fsm_step(sess, line); // transfer ownership of line
}

int session_do_read(struct session *sess)
{
    int rc, bufp = sess->buf_used;
    rc = read(sess->fd, sess->buf + bufp, inbufsize-bufp);
    if (rc <= 0) {
        sess->state = fsm_error;
        return 0;
    }

    sess->buf_used += rc;
    session_check_lf(sess);
    if (sess->buf_used == inbufsize) {
        session_send_str(sess, "Thou sayeth too much\n");
        sess->state = fsm_error;
        return 0;
    }

    if (sess->state == fsm_finish)
        return 0;
    return 1;
}

void log_session_result(struct session *sess, FILE *f)
{
    unsigned int ip = sess->from_ip;
    fprintf(f, "From %d.%d.%d.%d:%hd\n"
            "name: %s, age: %d, town: %s, band: %s\n",
            (ip >> 24), (ip >> 16) & 255, (ip >> 8) & 255, ip & 255, 
            sess->from_port, sess->name, sess->age, sess->town, sess->band);
    fflush(f);
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
    if (bind(sock, (struct sockaddr *) &addr, sizeof(addr)) == -1) {
        perror("bind");
        return 0;
    }

    listen(sock, LISTEN_QLEN);
    serv->ls = sock;

    serv->sessions = calloc(INIT_SESS_ARR_SIZE, sizeof(*serv->sessions));
    serv->sessions_size = INIT_SESS_ARR_SIZE;

    return 1;
}

void server_accept_client(struct server *serv)
{
    int sd, i;
    struct sockaddr_in addr;
    socklen_t len = sizeof(addr);
    sd = accept(serv->ls, (struct sockaddr *) &addr, &len);
    if (sd == -1) {
        perror("accept");
        return;
    }

    if (sd >= serv->sessions_size) { // resize if needed
        int newsize = serv->sessions_size;
        while (newsize <= sd)
            newsize += INIT_SESS_ARR_SIZE;
        serv->sessions = 
            realloc(serv->sessions, newsize * sizeof(*serv->sessions));
        for (i = serv->sessions_size; i < newsize; i++)
            serv->sessions[i] = NULL;
        serv->sessions_size = newsize;
    }

    serv->sessions[sd] = make_session(sd, addr.sin_addr.s_addr, addr.sin_port);
}

void server_remove_session(struct server *serv, int sd)
{
    close(sd);
    cleanup_session(serv->sessions[sd]);
    free(serv->sessions[sd]);
    serv->sessions[sd] = NULL;
}

void server_close_session(struct server *serv, int sd)
{
    if (serv->sessions[sd]->state == fsm_finish)
        log_session_result(serv->sessions[sd], stdout);
    server_remove_session(serv, sd);
}

int main() 
{
    // write main loop
}
