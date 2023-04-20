/* 6_13.c */
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
#define streq(s1, s2) strcmp(s1, s2) == 0

enum { 
    listen_queue_len = 16, 
    init_sessions_cap = 32,
    session_bufsize = 64
};

enum command_type { up, down, show, unknown };

struct session_buf {
    char buf[session_bufsize];
    int buf_used;
};

const char ok_msg[] = "Ok\n";
const char unknown_msg[] = "textttunknown command\n";

char *extract_line_from_buf(char *buf, int *buf_used)
{
    char *res = NULL;
    int bufp = 0;
    if (*buf_used == 0)
        return 0;

    while (bufp < *buf_used) {
        if (buf[bufp] == '\n') {
            res = malloc(bufp+1);
            strncpy(res, buf, bufp);
            res[bufp] = '\0';

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

    return res;
}

int char_is_space(char c)
{
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

char *get_stripped_str(const char *str)
{
    const char *start = str;
    const char *end = str + strlen(str);
    char *res;

    while (char_is_space(*start))
        start++;
    if (start == end) {
        res = malloc(1);
        *res = '\0';
        return res;
    }

    do {
        end--;
    } while (char_is_space(*end));
    end++;

    size_t len = end-start;
    res = malloc(len+1);
    strncpy(res, start, len);
    return res;
}

enum command_type get_command_type(char *cmd)
{ 
    enum command_type type;
    char *stripped = get_stripped_str(cmd);

    if (streq(stripped, "up"))
        type = up;
    else if (streq(stripped, "down"))
        type = down;
    else if (streq(stripped, "show"))
        type = show;
    else
        type = unknown;

    free(stripped);
    return type;
}

int main(int argc, char **argv) 
{
    int result = 0;
    int serv_val = 0;

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
    struct session_buf **sessions = calloc(sessions_cap, sizeof(*sessions));
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
                    realloc(sessions, new_cap * sizeof(*sessions));
                for (int i = sessions_cap; i < new_cap; i++)
                    sessions[i] = NULL;
                sessions_cap = new_cap;
            }

            sessions[fd] = malloc(sizeof(*sessions));
            sessions[fd]->buf_used = 0;
        }

        for (fd = 0; fd < sessions_cap; fd++) {
            struct session_buf *s = sessions[fd];
            if (s && FD_ISSET(fd, &readfds)) {
                int bufp = s->buf_used;
                int rc = read(fd, s->buf + bufp, session_bufsize-bufp);
                if (rc <= 0) {
                    close(fd);
                    free(s);
                    sessions[fd] = NULL;
                } else {
                    char *line;

                    s->buf_used += rc;

                    while ((line = extract_line_from_buf(s->buf, &s->buf_used))) {
                        switch (get_command_type(line)) {
                            case up:
                                serv_val++;
                                write(fd, ok_msg, sizeof(ok_msg)-1);
                                break;
                            case down:
                                serv_val--;
                                write(fd, ok_msg, sizeof(ok_msg)-1);
                                break;
                            case show: {
                                    char numbuf[12];
                                    sprintf(numbuf, "%d\n", serv_val);
                                    write(fd, numbuf, strlen(numbuf));
                                } break;
                            case unknown:
                                write(fd, unknown_msg, sizeof(unknown_msg)-1);
                                break;
                        }

                        free(line);
                    }
                }
            }
        }
    }


defer:
    if (ls != -1) close(ls);
    return result;
}
