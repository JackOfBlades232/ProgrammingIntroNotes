/* 4_23/2_47_remake.c */
#include <stdio.h>

int main (int argc, char **argv)
{
    FILE *fp;
    int c;

    if (argc < 2) {
        fprintf(stderr, "Provide source file name as cli arg!\n");
        return 1;
    }

    fp = fopen(argv[1], "r");
    if (!fp) {
        perror(argv[1]);
        return 2;
    }

    while ((c = fgetc(fp)) != EOF)
        putchar(c);
    
    return 0;
}
