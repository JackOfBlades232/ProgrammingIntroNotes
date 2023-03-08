/* 5_22.c */
#include <signal.h>
#include <unistd.h>
#include <errno.h>

enum {
    msg_size = 3
};

volatile static sig_atomic_t use_plus = 1;

volatile static sig_atomic_t used_int = 0;
volatile static sig_atomic_t int_counter = 0;

const char plus[msg_size] = "+\n";
const char minus[msg_size] = "-\n";

const char *cur_msg()
{
    return use_plus ? plus : minus;
}

void int_handler(int s)
{
    int save_errno = errno;
    signal(SIGINT, int_handler);
    used_int = 1;
    use_plus = 0;
    write(1, cur_msg(), msg_size-1);
    errno = save_errno;
}

void quit_handler(int s)
{
    int save_errno = errno;
    signal(SIGQUIT, quit_handler);
    use_plus = 1;
    write(1, cur_msg(), msg_size-1);
    errno = save_errno;
}

void alarm_handler(int s)
{
    int save_errno = errno;
    signal(SIGALRM, alarm_handler);
    write(1, cur_msg(), msg_size-1);
    errno = save_errno;
}

int main()
{
    signal(SIGINT, int_handler);
    signal(SIGQUIT, quit_handler);
    signal(SIGALRM, alarm_handler);
    for (alarm(1);;) {
        pause();
        alarm(1);
        if (used_int)
            int_counter++;
        else
            int_counter = 0;

        if (int_counter >= 2) {
            alarm(0);
            break;
        }

        used_int = 0;
    }

    return 0;
}
