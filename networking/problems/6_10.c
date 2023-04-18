/* 6_10.c */
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <sys/select.h>
#include <time.h>

enum { delay = 5 /* seconds */ };

volatile static sig_atomic_t sigints_caught = 0;

void int_handler(int s)
{
    signal(SIGINT, int_handler);
    sigints_caught++;
}

int main()
{
    int num_lines = 0, num_chars = 0;
    sigset_t mask, orig_mask;

    signal(SIGINT, int_handler);
    sigemptyset(&mask);
    sigaddset(&mask, SIGINT);
    sigprocmask(SIG_BLOCK, &mask, &orig_mask);

    for (;;) {
        fd_set readfds;
        struct timespec timeout = { 5, 0 };

        FD_ZERO(&readfds);
        FD_SET(0, &readfds); // stdin

        int sel_res = pselect(1, &readfds, NULL, NULL, &timeout, &orig_mask);
        if (sel_res == 1) {
            char buf[1];
            sigints_caught = 0;
            if (read(0, buf, 1) == 0)
                break;
            else {
                if (*buf == '\n')
                    num_lines++;
                else
                    num_chars++;
            }
        } else if (sel_res == 0) {
            sigints_caught = 0;
            printf("You alive there, bruv?\n");
        } else if (errno == EINTR) {
            if (sigints_caught == 1)
                printf("Chars: %d, lines: %d\n", num_chars, num_lines);
            else if (sigints_caught > 1)
                break;
        } else {
            perror("select");
            return -1;
        }
    }

    return 0;
}
