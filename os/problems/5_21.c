/* 5_21.c */
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>

volatile static sig_atomic_t n_ints = 0;
volatile static sig_atomic_t to_print = 0;

void int_handler(int s)
{
    signal(SIGINT, int_handler);
    n_ints++;
}

void usr1_handler(int s)
{
    signal(SIGUSR1, usr1_handler);
    to_print = 1;
}

int main()
{
    signal(SIGINT, int_handler);
    signal(SIGUSR1, usr1_handler);
    for (;;) {
        if (to_print) {
            to_print = 0;
            printf("%d\n", n_ints);
        }

        pause();
    }
    return 0;
}
