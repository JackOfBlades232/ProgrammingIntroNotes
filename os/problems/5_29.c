/* 5_29.c */
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

void collect_stats(FILE *f, int *char_cnt, int *line_cnt)
{
    int c;
    while ((c = getc(f)) != EOF) {
        if (c == '\n')
            (*line_cnt)++;
        else
            (*char_cnt)++;
    }
}

int main(int argc, char **argv)
{
    int prog_pid;
    int status, wr;
    int in_fd[2], out_fd[2];

    int i;
    int write_res;

    if (argc < 2) {
        fprintf(stderr, "Provide a program as cli arg to launch\n");
        return 1;
    }

    pipe(in_fd);
    pipe(out_fd);
    
    prog_pid = fork();
    if (prog_pid == 0) { /* prog proc */
        dup2(in_fd[1], STDOUT_FILENO);
        dup2(out_fd[0], STDIN_FILENO);
        close(in_fd[0]);
        close(in_fd[1]);
        close(out_fd[0]);
        close(out_fd[1]);

        execvp(argv[1], argv+1);
        perror(argv[1]);
        _exit(1);
    }

    close(in_fd[1]);
    close(out_fd[0]);

    if (fork() == 0) { /* stats collecting */
        int char_cnt = 0, line_cnt = 0;

        dup2(in_fd[0], STDIN_FILENO);
        close(out_fd[1]);

        collect_stats(stdin, &char_cnt, &line_cnt);
        printf("%d %d\n", char_cnt, line_cnt);

        _exit(0);
    }

    close(in_fd[0]);

    signal(SIGPIPE, SIG_IGN); 
    for (i = min_i; i <= max_i; i++) {
        write_res = dprintf(out_fd[1], "%d\n", i);
        if (write_res < 0) {
            printf("Could not feed all numbers to given program\n");
            break;
        }
    }

    close(out_fd[1]);
    
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
