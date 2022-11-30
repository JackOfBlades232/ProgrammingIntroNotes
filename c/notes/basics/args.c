#include <stdio.h>

/* last elem in argv is NULL */
int main(int argc, char **argv)
{
    int i;
    printf("My name is %s\n", argv[0]);
    for (i = 1; i < argc; i++)
        printf("[%s]\n", argv[i]);
    return 0;
}
