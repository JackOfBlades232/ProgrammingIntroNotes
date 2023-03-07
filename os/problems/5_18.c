/* 5_18.c */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#define D

int main(int argc, char **argv)
{
    int fd, fd_dest;
    int pid;
    int wr, status;

    if (argc < 3) {
        fprintf(stderr, "Provide file name and program to run\n");
        return 1;
    }

#if defined(A)
    fd_dest = STDOUT_FILENO;
    fd = open(argv[1], O_WRONLY|O_CREAT|O_TRUNC, 0666);
#elif defined(B)
    fd_dest = STDIN_FILENO;
    fd = open(argv[1], O_RDONLY);
#elif defined(C)
    fd_dest = STDOUT_FILENO;
    fd = open(argv[1], O_WRONLY|O_CREAT|O_APPEND, 0666);
#elif defined(D)
    fd_dest = STDOUT_FILENO;
    fd = open(argv[1], O_WRONLY|O_APPEND);
#endif

    if (fd == -1) {
        fprintf(stderr, "Could not open file\n");
        return 2;
    }

    pid = fork();
    if (pid == 0) { /* child proc */
        dup2(fd, fd_dest);
        close(fd);
        execvp(argv[2], argv+2);
        perror(argv[2]);
        exit(1);
    }

    close(fd);

    wr = wait(&status);
    if (wr == -1 || !WIFEXITED(status) || WEXITSTATUS(status) != 0) {
        return 3;
    }

    return 0;
}
