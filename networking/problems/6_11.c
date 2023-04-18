/* 6_11.c */
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/select.h>

#define return_defer(code) do { result = code; goto defer; } while (0);

enum { bufsize = 256 };

int open_fifo(const char *filenm, int flags)
{
    int fd = open(filenm, flags);
    if (fd == -1)
        fd = mkfifo(filenm, 0666);
    return fd;
}

int main(int argc, char **argv)
{
    int result = 0;
    int in_fd = -1, out_fd = -1;

    if (argc != 3) {
        fprintf(stderr, "Usage: <in_fifonm> <out_fifonm>\n");
        return_defer(1);
    }

    in_fd = open_fifo(argv[1], O_RDONLY|O_NONBLOCK); // to avoid mutual blocking
    if (in_fd == -1) {
        perror(argv[1]);
        return_defer(2);
    }
    out_fd = open_fifo(argv[2], O_WRONLY);
    if (out_fd == -1) {
        perror(argv[2]);
        return_defer(2);
    }
        
    for (;;) {
        fd_set readfds;
        FD_ZERO(&readfds);
        FD_SET(0, &readfds);
        FD_SET(in_fd, &readfds);

        int sr = select(2, &readfds, NULL, NULL, NULL);
        if (sr == -1) {
            perror("select");
            return_defer(3);
        }

        char stdin_buf[bufsize];
        int read_res; 
        if (FD_ISSET(0, &readfds)) {
            read_res = read(0, stdin_buf, sizeof(stdin_buf));
            if (read_res == -1) {
                perror("stdin");
                return_defer(4);
            } else if (read_res == 0) {
                return_defer(0);
            } else {
                // TODO: process write_res?
                write(out_fd, stdin_buf, read_res);
            }
        }
        if (FD_ISSET(in_fd, &readfds)) {
            read_res = read(in_fd, stdin_buf, sizeof(stdin_buf));
            if (read_res == -1) {
                perror("fifo");
                return_defer(4);
            } else if (read_res == 0) {
                printf("Nobody writing anymore\n");
                return_defer(0);
            } else
                write(1, stdin_buf, read_res);
        }
    }

defer:
    if (in_fd != -1) close(in_fd);
    if (out_fd != -1) close(out_fd);
    return result;
}
