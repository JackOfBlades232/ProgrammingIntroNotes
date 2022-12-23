/* 4_23/2_49_remake.c */
#include <stdio.h>

int main(int argc, char **argv)
{
    FILE *fp;
    int c;
  
    if (argc < 2) {
        fprintf(stderr, "Provide destination file name!\n");
        return 1;
    }

    fp = fopen(argv[1], "w");
    if (!fp) {
        perror(argv[1]);
        return 2;
    }

    while ((c = getchar()) != EOF)
        fputc(c, fp);

    fclose(fp);
    return 0;
}
