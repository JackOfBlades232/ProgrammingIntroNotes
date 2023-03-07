/* 5_12.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int main(int argc, char **argv)
{
    int pid;
    int status, wr;

    if (argc < 2) {
        fprintf(stderr, "Provide a program as cli arg to launch\n");
        return 1;
    }
    
    pid = fork();
    if (pid == 0) {
        execvp(argv[1], argv+1);
        perror(argv[1]);
        _exit(1);
    }

    wr = wait(&status);
    if (wr == -1) {
        fprintf(stderr, "Failed to launch given program\n");
        return 2;
    } else if (WIFEXITED(status))
        printf("exited %d\n", WEXITSTATUS(status));
    else
        printf("killed %d\n", WTERMSIG(status));

    return 0;
}
