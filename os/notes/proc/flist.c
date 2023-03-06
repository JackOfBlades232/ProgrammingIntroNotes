/* proc/flist.c */
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <unistd.h>

int main()
{
    /* emulate ls -l -a -R / > flist.txt */
    int pid, status;
    pid = fork();
    if (pid == 0) {
        /* child */
        int fd = open("flist.txt", O_CREAT|O_WRONLY|O_TRUNC, 0666);
        if (fd == -1) {
            perror("flist.txt");
            exit(1);
        }
        dup2(fd, 1);
        close(fd);
        execlp("ls", "ls", "-l", "-a", "-R", "/", NULL);
        perror("ls");
        exit(2);
    }
    
    /* parent */
    wait(&status);
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
        fprintf(stderr, "Child proc failed\n");
        return 3;
    }

    return 0;
}
