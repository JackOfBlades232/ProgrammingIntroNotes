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
    }
    return 0;
}
