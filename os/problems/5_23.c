/* 5_22.c */
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>

enum {
    delay = 5 /* seconds */
};

volatile static sig_atomic_t int_presses = 0;
volatile static sig_atomic_t n_chars = 0;
volatile static sig_atomic_t n_lines = 0;

const char msg[] = "You alive there, bruv?\n";

void int_handler(int s);
void alarm_handler(int s);

static const struct sigaction int_action = { int_handler };
static const struct sigaction alarm_action = { alarm_handler };

void int_handler(int s)
{
    sigaction(SIGINT, &int_action, NULL);
    int_presses++;
}

void alarm_handler(int s)
{
    int save_errno = errno;
    sigaction(SIGALRM, &alarm_action, NULL);
    int_presses = 0;
    write(1, msg, sizeof(msg)-1);
    errno = save_errno;
}

void update_counters(char c)
{
    if (c == '\n' || c == '\r')
        n_lines++;
    else
        n_chars++;
}

int main()
{
    int read_res = 1;
    char buf[1];

    sigaction(SIGINT, &int_action, NULL);
    sigaction(SIGALRM, &alarm_action, NULL);
    while (read_res != 0) {
        alarm(5);
        read_res = read(0, buf, sizeof(buf));    
        if (read_res > 0) {
            int_presses = 0;
            update_counters(*buf);
        } else if (read_res == -1) {
            if (int_presses == 1)
                printf("Chars: %d; lines: %d\n", n_chars, n_lines);
            else if (int_presses > 1) {
                alarm(0);
                break;
            }
        }
    }

    return 0;
}
