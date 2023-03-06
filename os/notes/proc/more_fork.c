/* proc/more_fork.c */
#include <unistd.h>
#include <stdio.h>

int main()
{
    int p;

    p = fork();
    if (p == 0)
        printf("I'm the child\n");
    else
        printf("I'm the parent\n");

    return 0;
}
