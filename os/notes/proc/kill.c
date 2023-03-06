/* proc/kill.c */
#include <sys/types.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>

void handler(int s) /* handlers get called async-ly */
{
    printf("I've intercepted your signal!\n");
}

int main()
{
    if(signal(SIGTERM, &handler) != SIG_ERR) { /* overload signal handling */
        kill(getpid(), SIGTERM); /* syscall for sending signal to proc */
        /* negative pid vals stand for all signals and groups, a non-su proc can
         * only send signals to same-user procs */
        signal(SIGTERM, SIG_DFL); /* also spec vals: here, restore def */
        /* however, usually when a signal comes the handler is auto-reset,
         * so users tend to put another "signal" call as first line of
         * the handler. Linux may use the bsd-version in signal as a func
         * (BSD ver does not reset handler and blocks signal until handler
         * has finished), and the syscall will be sigaction */

        /* problem with handlers : they are called by the kernel, so to pass
         * data one has to use global variables */

        /* another: the signal may come at any moment, so it can lead to bad
         * results, for example: if the program is running malloc or free, and
         * the handler has malloc or free, it may corrupt the heap. 
         * Similarly, high level io should not be used in handlers (cause of
         * buffers). One can still do io with syscalls, but errno may be
         * overwritten (if handler is called after a syscall and before it's
         * error handling. */

        /* tools for negating damage: volatile keyword (the compiler will know,
         * that this var can change unexpectedly), sig_atomic_t type (a type 
         * that is always changed in 1 machine instr, usually int) */

        /* also, signals interrupt blocking syscalls, like read and sleep,
         * and these syscall return -1 (you can still detect this situation by
         * checking errno (it will be eq to EINTR) */
    }
    return 0;
}
