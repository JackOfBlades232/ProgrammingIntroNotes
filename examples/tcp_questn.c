#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>


#ifndef INBUFSIZE
#define INBUFSIZE 1024
#endif

#ifndef LISTEN_QLEN
#define LISTEN_QLEN 32
#endif

#ifndef INIT_SESS_ARR_SIZE
#define INIT_SESS_ARR_SIZE 32
#endif

enum fsm_states {
    fsm_start, fsm_name = fsm_start, fsm_age, fsm_town, fsm_band,
    fsm_finish, fsm_error
};

struct session {
    int fd;
    unsigned long from_ip;
    unsigned short from_port;
    char buf[INBUFSIZE];
    int buf_used;
    enum fsm_states state;
    char *name, *town, *band;
    int age;
};

static void session_send_string(struct session *sess, const char *str)
{
    write(sess->fd, str, strlen(str));
}

static struct session *make_new_session(int fd, struct sockaddr_in *from)
{
    struct session *sess = malloc(sizeof(*sess));
    sess->fd = fd;
    sess->from_ip = ntohl(from->sin_addr.s_addr);
    sess->from_port = ntohs(from->sin_port);
    sess->buf_used = 0;
    sess->state = fsm_start;
    sess->name = NULL;
    sess->town = NULL;
    sess->band = NULL;
    sess->age = -1;
    session_send_string(sess, "What is your name?\n");
    return sess;
}

static void session_cleanup(struct session *sess)
{
    if(sess->name)
        free(sess->name);
    if(sess->town)
        free(sess->town);
    if(sess->band)
        free(sess->band);
}

static void session_handle_age(struct session *sess, const char *line)
{
    char *err;
    int age;
    age = strtol(line, &err, 10);
    if(!*line || *err || age < 0 || age > 150) {
        session_send_string(sess, "No, this can't be your age!\n");
        session_send_string(sess, "Please try again!\n");
        session_send_string(sess, "How old are you?\n");
    } else {
        sess->age = age;
        session_send_string(sess, "What city/town are you from?\n");
        sess->state = fsm_town;
    }
}

/* ownership over the 'line' is transferred here */
static void session_fsm_step(struct session *sess, char *line)
{
    switch(sess->state) {
    case fsm_name:
        sess->name = line;
        session_send_string(sess, "How old are you?\n");
        sess->state = fsm_age;
        break;
    case fsm_age:
        session_handle_age(sess, line);
        free(line); /* because we still own it... */
        break;
    case fsm_town:
        sess->town = line;
        session_send_string(sess, "What is your favorite band?\n");
        sess->state = fsm_band;
        break;
    case fsm_band:
        sess->band = line;
        session_send_string(sess, "Thank you, bye!\n");
        sess->state = fsm_finish;
        break;
    case fsm_finish:
    case fsm_error:
        free(line);   /* this should never happen */
    }
}

static void session_check_lf(struct session *sess)
{
    int pos = -1;
    int i;
    char *line;
    for(i = 0; i < sess->buf_used; i++) {
        if(sess->buf[i] == '\n') {
            pos = i;
            break;
        }
    }
    if(pos == -1)
        return;
    line = malloc(pos+1);
    memcpy(line, sess->buf, pos);
    line[pos] = 0;
    sess->buf_used -= (pos+1);
    memmove(sess->buf, sess->buf+pos+1, sess->buf_used);
    if(line[pos-1] == '\r')
        line[pos-1] = 0;
    session_fsm_step(sess, line);  /* we transfer ownership! */
}

static int session_do_read(struct session *sess)
{
    int rc, bufp = sess->buf_used;
    rc = read(sess->fd, sess->buf + bufp, INBUFSIZE - bufp);
    if(rc <= 0) {
        sess->state = fsm_error;
        return 0;   /* this means "don't continue" for the caller */
    }
    sess->buf_used += rc;
    session_check_lf(sess);
    if(sess->buf_used >= INBUFSIZE) {
        /* we can't read further, no room in the buffer, no whole line yet */
        session_send_string(sess, "Line too long! Good bye...\n");
        sess->state = fsm_error;
        return 0;
    }
    if(sess->state == fsm_finish)
        return 0;
    return 1;
}

static void session_commit(struct session *sess, FILE *f)
{
    unsigned int ip = sess->from_ip;
    fprintf(f, "From %d.%d.%d.%d:%d\n"
               "Name: %s\nAge: %d\nTown: %s\nBand: %s\n\n",
               (ip>>24 & 0xff), (ip>>16 & 0xff), (ip>>8 & 0xff), (ip & 0xff),
               sess->from_port,
               sess->name, sess->age, sess->town, sess->band);
    fflush(f);
}

/* =========== server =========== */

struct server_str {
    int ls;
    FILE *res;
    struct session **sess_array;
    int sess_array_size;
};

static int server_init(struct server_str *serv, int port, const char *fname)
{
    int sock, i, opt;
    struct sockaddr_in addr;
    FILE *f;

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if(sock == -1) {
        perror("socket");
        return 0;
    }
    opt = 1;
    setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(port);
    if(-1 == bind(sock, (struct sockaddr*) &addr, sizeof(addr))) {
        perror("bind");
        return 0;
    }

    listen(sock, LISTEN_QLEN);

    serv->ls = sock;

    f = fopen(fname, "wb");
    if(!f) {
        perror(fname);
        close(sock);
        return 0;
    }
    serv->res = f;

    serv->sess_array = malloc(sizeof(*serv->sess_array) * INIT_SESS_ARR_SIZE);
    serv->sess_array_size = INIT_SESS_ARR_SIZE;
    for(i = 0; i < INIT_SESS_ARR_SIZE; i++)
        serv->sess_array[i] = NULL;

    return 1;
}

static void server_accept_client(struct server_str *serv)
{
    int sd, i;
    struct sockaddr_in addr;
    socklen_t len = sizeof(addr);
    sd = accept(serv->ls, (struct sockaddr*) &addr, &len);
    if(sd == -1) {
        perror("accept");
        return;
    }

    if(sd >= serv->sess_array_size) {  /* need to resize */
        int newlen = serv->sess_array_size;
        while(sd >= newlen)
            newlen += INIT_SESS_ARR_SIZE;
        serv->sess_array =
            realloc(serv->sess_array, newlen * sizeof(struct session*));
        for(i = serv->sess_array_size; i < newlen; i++)
            serv->sess_array[i] = NULL;
        serv->sess_array_size = newlen;
    }

    serv->sess_array[sd] = make_new_session(sd, &addr);
}

static void server_remove_session(struct server_str *serv, int sd)
{
    close(sd);
    serv->sess_array[sd]->fd = -1;
    session_cleanup(serv->sess_array[sd]);
    free(serv->sess_array[sd]);
    serv->sess_array[sd] = NULL;
}

static void server_close_session(struct server_str *serv, int sd)
{
    if(serv->sess_array[sd]->state == fsm_finish)
        session_commit(serv->sess_array[sd], serv->res);
    server_remove_session(serv, sd);
}

static int server_go(struct server_str *serv)
{
    fd_set readfds;
    int i, sr, ssr, maxfd;
    for(;;) {   /* ========= THE APPLICATION MAIN LOOP ========= */
        FD_ZERO(&readfds);        /* ====== GET THE EVENT ====== */
        FD_SET(serv->ls, &readfds);
        maxfd = serv->ls;
        for(i = 0; i < serv->sess_array_size; i++) {
            if(serv->sess_array[i]) {
                FD_SET(i, &readfds);
                if(i > maxfd)
                    maxfd = i;
            }
        }
        sr = select(maxfd+1, &readfds, NULL, NULL, NULL);
        if(sr == -1) {
            perror("select");
            return 4;
        }
        if(FD_ISSET(serv->ls, &readfds))
            server_accept_client(serv);
        for(i = 0; i < serv->sess_array_size; i++) {
            if(serv->sess_array[i] && FD_ISSET(i, &readfds)) {
                ssr = session_do_read(serv->sess_array[i]);
                if(!ssr)
                    server_close_session(serv, i);
            }
        }
    }
    return 0;
}

int main(int argc, const char * const *argv)
{
    struct server_str server;
    long port;
    char *endptr;

    if(argc != 3) {
        fprintf(stderr, "Usage: serv <port> <log_file_name>\n");
        return 1;
    }

    port = strtol(argv[1], &endptr, 10);
    if(!*argv[1] || *endptr) {
        fprintf(stderr, "Invalid port number\n");
        return 2;
    }

    if(!server_init(&server, port, argv[2]))
        return 3;

    return server_go(&server);
}
