/* 4_10.c */
#include <stdio.h>

int main(int argc, char **argv)
{
    int i;
    for (i = 1; i < argc; i++) {
        if (argv[i][0] != '-')
            printf("[%s]\n", argv[i]);
    }
    return 0;
}
