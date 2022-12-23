/* 4_23/2_48_remake.c */
#include <stdio.h>

int main (int argc, char **argv)
{
    FILE *fp;
    int c, num_lines;

    if (argc < 2) {
        fprintf(stderr, "Provide source file name as cli arg!\n");
        return 1;
    }

    fp = fopen(argv[1], "r");
    if (!fp) {
        perror(argv[1]);
        return 2;
    }

    num_lines = 0;
    while ((c = fgetc(fp)) != EOF) {
        if (c == 10)
            num_lines++;
    }

    printf("%d\n", num_lines);
    
    return 0;
}
