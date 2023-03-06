/* proc/conv.c */
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

/* let us emulate ls -lR | grep '^d' */

int main()
{
    int fd[2];
    pipe(fd);

    if (fork() == 0) { /* ls -lR proc */
        close(fd[0]);
        dup2(fd[1], 1); /* set stdout to channel */
        close(fd[1]);
        execlp("ls", "ls", "-lR", NULL); /* run command */
        perror("ls"); /* if not switched, error */
        exit(1);
    }

    if (fork() == 0) { /* grep '^d' proc */
        close(fd[1]);
        dup2(fd[0], 0); /* set stdin to channel */
        close(fd[0]);
        execlp("grep", "grep", "^d", NULL); /* run command */
        perror("grep"); /* if not switched, error */
        exit(1);
    }

    /* remove descriptors in parent and wait for both children */
    close(fd[0]);
    close(fd[1]);
    wait(NULL);
    wait(NULL);
    return 0;
}
