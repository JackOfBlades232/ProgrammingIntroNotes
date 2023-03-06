/* proc/wait.c */
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>

int main()
{
    int status, wr;
    fork();
    wr = wait(&status); /* also, wait4 for finer control */
    if (wr == -1)
        printf("There are no child processes\n");
    else {
        printf("Process with pid=%d finished.\n", wr);
        if (WIFEXITED(status)) /* check if finished by exit syscall */
            printf("It has exited with code=%d.\n", WEXITSTATUS(status));
        else 
            printf("It was killed by signal %d.\n", WTERMSIG(status));
    }

    return 0;
}
