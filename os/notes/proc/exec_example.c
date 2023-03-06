/* proc/exec_example.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int main()
{
    int pid;
    pid = fork();
    if (pid == -1) {
        perror("fork");
        exit(1);
    } else if (pid == 0) { 
        /* child proc */
        execlp("ls", "ls", "-l", "-a", "/var", NULL);
        perror("ls"); /* if reached, exec had an error */
        exit(1);
    }

    /* parent proc */
    wait(NULL); /* wait and kill zombie */

    printf("OK\n");
    return 0;
}
