/* fgetputc.c */
/* copy no more than 10 lines from text file to text file */
#include <stdio.h>

int main(int argc, char **argv)
{
    FILE *from, *to;
    int c, lcnt;

    if (argc < 3) {
        fprintf(stderr, "Too few args\n");
        return 1;
    }

    from = fopen(argv[1], "r");
    if (!from) {
        perror(argv[1]);
        return 2;
    }
    to = fopen(argv[2], "w");
    if (!to) {
        perror(argv[2]);
        return 3;
    }

    lcnt = 0;
    while ((c = fgetc(from)) != EOF) {
        fputc(c, to);
        if (c == '\n')
            lcnt++;
        if (lcnt >= 10)
            break;
    }
    
    /* c will close the descriptors for us */
    return 0;
}
