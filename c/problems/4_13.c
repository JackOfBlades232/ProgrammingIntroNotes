/* 4_13.c */
#include <stdio.h>

void put_next_square_in_string(char *str, long *n, long long *n_sq)
{
    *n_sq += *n + *n + 1;
    (*n)++;
    sprintf(str, "%lld", *n_sq);
}

void print_square_sequence_digits(int start, int end)
{
    long n;
    long long n_sq;
    char dgts[20];
    const char *dp;
    int idx;

    idx = 0;
    n = 0;
    n_sq = 0;
    for (;;) {
        put_next_square_in_string(dgts, &n, &n_sq);
        for (dp = dgts; *dp; dp++, idx++)
            if (idx >= end)
                goto fin;
            else if (idx >= start)
                putchar(*dp);
    }

fin:
    putchar('\n');
}

int main(int argc, char **argv)
{
    int start, end;

    if (argc != 3) {
        fprintf(stderr, "Provide 2 args: start and end index.\n");
        return 1;
    }
        
    sscanf(argv[1], "%d", &start);
    sscanf(argv[2], "%d", &end);
    start--;
    print_square_sequence_digits(start, end);

    return 0;
}
