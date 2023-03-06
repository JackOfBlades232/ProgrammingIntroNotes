/* proc/procmask.c */
#include <bits/types/sigset_t.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>

volatile sig_atomic_t child_ready;
void usr1hdl(int n)
{
    child_ready = 1;
}

int main()
{
    int pid;

    /* remedy for while-pause bug */
    sigset_t mask_usr1, mask_empty;
    sigemptyset(&mask_usr1); /* prep set with only usr1 to block */
    sigaddset(&mask_usr1, SIGUSR1);
    sigemptyset(&mask_empty); /* prep all-signals-allowed set */

    /* prep main proc */
    child_ready = 0;
    /* by default block usr1 signals (outside of waiting in while)
     * all these signals will be buffered until allowed */
    sigprocmask(SIG_SETMASK, &mask_usr1, NULL);
    
    signal(SIGUSR1, usr1hdl);
    pid = fork();
    if (pid == 0) { /* child proc */
        /* prep */
        kill(getppid(), SIGUSR1); /* tell main proc that child prep over */
        /* main work */
        exit(0);
    }
    /* cont parent proc when child ready (set with signal) */

    /* while (!child_ready)
        pause(); */
    /* this way is not good: if handler gets called between while and pause(),
     * the proc will be stuck in pause, waiting for some other signal to come
     * and break pause, so that it can check cond again */
    /* One way to handle this would be to not use signals like this */

    while (!child_ready)
        sigsuspend(&mask_empty); /* allow (and handle buffered) usr1 signals
                                    only for the time of waiting in suspend */

    /* through these complicated precautions the bug is fixed. However, this
     * only speaks against using signals for inter-proc communication */

    /* now child is ready */
    return 0;
}
