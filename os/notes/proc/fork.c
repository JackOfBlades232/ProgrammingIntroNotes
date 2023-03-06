/* proc/fork.c */
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>

int main()
{
    int p;
    fork(); /* fork creates a runnning copy of the proc with ppid = this pid */
    fork(); /* and return the new proc pid (or 0/-1). It starts running from */
    p = fork(); /* the fork call */
    if (p == -1)
        printf("Err\n");
    else if (p == 0) /* 0 is returned for spawned proc */
        printf("Spawn\n");
    printf("Hello\n");
    return 0;
}
