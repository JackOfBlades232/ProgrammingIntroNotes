/* 6_12.c */
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

#define return_defer(res) result = res; goto defer
#define max(a, b) (a > b ? a : b) 

enum { 
    listen_queue_len = 16, 
    init_sessions_cap = 32,
    session_bufsize = 64
};

struct session {
    char buf[session_bufsize];
    int buf_used;
};

const char msg[] = "Ok\n";

int read_buffer_and_cut_line(char *buf, int *buf_used)
{
    if (*buf_used == 0)
        return 0;

    int bufp = 0;
    int found = 0;
    while (bufp < *buf_used) {
        if (buf[bufp] == '\n') {
            found = 1;
            bufp++;
            break;
        }

        bufp++;
    }

    if (bufp < *buf_used) {
        memmove(buf, buf+bufp, *buf_used-bufp);
        *buf_used -= bufp;
    } else
        *buf_used = 0;

    return found;
}

int main(int argc, char **argv) 
{
    int result = 0;

    int ls = -1;
    struct sockaddr_in addr;
    char *endptr;
    long port;

    if (argc != 2) {
        fprintf(stderr, "Args: <port>\n");
        return -1;
    }

    port = strtol(argv[1], &endptr, 10);
    if (!*argv[1] || *endptr) {
        fprintf(stderr, "Invalid port number\n");
        return -1;
    }

    ls = socket(AF_INET, SOCK_STREAM, 0);
    if (ls == -1) {
        fprintf(stderr, "Failed to open socket\n");
        return -1;
    }

    int opt = 1;
    setsockopt(ls, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    int ok = bind(ls, (struct sockaddr *) &addr, sizeof(addr));
    if (ok == -1) {
        fprintf(stderr, "Failed to bind socket\n");
        return_defer(-1);
    }

    listen(ls, listen_queue_len);

    int sessions_cap = init_sessions_cap;
    struct session **sessions = calloc(sessions_cap, sizeof(struct session *));

    for (;;) {
        int fd, res;
        fd_set readfds;
        int max_d = ls; 

        FD_ZERO(&readfds);
        FD_SET(ls, &readfds);

        for (fd = 0; fd < sessions_cap; fd++) {
            if (sessions[fd]) {
                FD_SET(fd, &readfds);
                if (fd > max_d)
                    max_d = fd;
            }
        }

        res = select(max_d+1, &readfds, NULL, NULL, NULL);
        
        if (res == -1) { 
            if (errno == EINTR) {
                printf("Connection closed\n");
                break;
            } else {
                perror("select");
                continue;
            }
        }

        if (FD_ISSET(ls, &readfds)) {
            socklen_t len;
            fd = accept(ls, (struct sockaddr *) &addr, &len);
            if (fd == -1) {
                perror("accept");
                continue;
            }

            if (fd >= sessions_cap) { // resize
                int new_cap = sessions_cap;
                while (fd >= new_cap)
                    new_cap += init_sessions_cap;
                sessions = 
                    realloc(sessions, new_cap * sizeof(struct session *));
                for (int i = sessions_cap; i < new_cap; i++)
                    sessions[i] = NULL;
                sessions_cap = new_cap;
            }

            sessions[fd] = malloc(sizeof(struct session));
            sessions[fd]->buf_used = 0;
        }

        for (fd = 0; fd < sessions_cap; fd++) {
            struct session *s = sessions[fd];
            if (s && FD_ISSET(fd, &readfds)) {
                int bufp = s->buf_used;
                int rc = read(fd, s->buf + bufp, session_bufsize-bufp);
                if (rc <= 0) {
                    close(fd);
                    free(s);
                    sessions[fd] = NULL;
                } else {
                    s->buf_used += rc;
                    while (read_buffer_and_cut_line(s->buf, &s->buf_used))
                        write(fd, msg, sizeof(msg)-1);
                }
            }
        }
    }


defer:
    if (ls != -1) close(ls);
    return result;
}
