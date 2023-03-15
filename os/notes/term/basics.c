/* basics.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main()
{
    int sid, pgid;
    int pid;

    if (isatty(0)) /* check if fd is a terminal */
        printf("Input is term!\n");
    else
        printf("Input is not a terminal\n");

    /* 0 == this proc */
    sid = getsid(0); /* for session leader =pid */
    pgid = getpgid(0); /* for group leader =pid */
    printf("sid: %d, pgid: %d\n", sid, pgid);
    
    tcsetpgrp(0, getpgid(getppid()));
    /* if controlling term exists, set p-group as current. Need fd, cause 
     * it is ioctl syscall under the hood */

    pid = fork();
    if (pid > 0)
        exit(0);
    setsid(); /* spawns another session and makes this proc it's leader
                 (and it's first group leader), won't work if proc already
                 leads something */

    setpgid(getpid(), getpid()); /* change pgid, either to existing group in
                                    session or, if =pid&not leader,
                                    create new one */

    printf("New sid: %d, pgid: %d\n", getsid(0), getpgid(0));

    /* if there is no controlling terminal for a session, it's leader can
     * open the file corresponding to the termnal. With open one can use
     * O_NOCTTY to force the terminal not to become controlling. */

    return 0;
}
