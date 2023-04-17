/* 6_9.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/select.h>

#define NAME_BUFSIZE 128

void remove_linefeed(char *s)
{
    for (char *sp = s; *sp; sp++) {
        if (*sp == '\n') {
            *sp = '\0';
            break;
        }
    }
}

int main()
{
    int result;
    fd_set readfds;
    struct timeval timeout = { 15, 0 };
    char name_buf[NAME_BUFSIZE];

    FD_ZERO(&readfds);
    FD_SET(0, &readfds); // stdin

    printf("What is your name, sire/madame?\n");
    result = select(1, &readfds, NULL, NULL, &timeout);
    switch (result) {
        case -1:
            if (errno == EINTR) {
                fprintf(stderr, "Interrupted by signal\n");
                return 0;
            } else {
                perror("select");
                return -1;
            } break;
        case 0:
            printf("Sorry, can't wait in you forever\n");
            break;
        default:
            if (fgets(name_buf, sizeof(name_buf), stdin)) {
                remove_linefeed(name_buf);
                printf("Nice to meet you, %s!\n", name_buf);
            } else {
                perror("input");
                return -1;
            }
            break;
    }

    return 0;
}
