/* proc/pressagain.c */
#include <signal.h>
#include <unistd.h>
#include <errno.h>

/* global var for sighandlers, protected from compiler opts */
volatile static sig_atomic_t n = 0;

const char message[] = "Try again, eventually you may kill me\n";

void handler(int s)
{
    int save_errno = errno; /* prevent messing up errno */
    signal(SIGINT, handler); /* in case it is reset, set handler again */
    n++;
    write(1, message, sizeof(message)-1); /* not to mess with lib buffers */
    errno = save_errno;
}

int main()
{
    signal(SIGINT, handler);
    while (n < 10) {
        // sleep(1); /* not to waste proc time on while */
        pause(); /* waits for signal */
    }
    return 0;
}
