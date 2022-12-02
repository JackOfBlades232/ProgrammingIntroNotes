/* eof.c */
#include <stdio.h>

int main()
{
    int c;
    c = getchar(); /* getchar returns int, not uns char, because it needs 256
                      values for the byte read (can read from anything
                      connected to stdin), and -1 for EOF */
    while ((c = getchar()) != EOF) {
        if (c == '\n')
            printf("OK\n");
    }

    return 0;
}
