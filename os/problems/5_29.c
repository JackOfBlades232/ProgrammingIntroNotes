/* 5_27.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>

enum {
    min_i = 1,
    max_i = 1000000
};

int main(int argc, char **argv)
{
    int pid;
    int status, wr;
    int fd[2];

    int i;
    int write_res;

    if (argc < 2) {
        fprintf(stderr, "Provide a program as cli arg to launch\n");
        return 1;
    }

    pipe(fd);
    
    pid = fork();
    if (pid == 0) {
        dup2(fd[0], STDIN_FILENO);
        close(fd[0]);
        close(fd[1]);

        execvp(argv[1], argv+1);
        perror(argv[1]);
        _exit(1);
    }

    close(fd[0]);

    signal(SIGPIPE, SIG_IGN); 
    for (i = min_i; i <= max_i; i++) {
        write_res = dprintf(fd[1], "%d\n", i);
        if (write_res < 0) {
            printf("Could not feed all numbers to given program\n");
            break;
        }
    }

    close(fd[1]);

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
