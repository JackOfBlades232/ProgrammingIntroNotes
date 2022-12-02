/* intsum.c */
#include <stdio.h>

void intfilesum(FILE *f, int *sum, int *count)
{
    int n;

    *count = 0;
    *sum = 0;
    while (fscanf(f, "%d", &n) == 1) {
        *sum += n;
        (*count)++;
    }
}

int main()
{
    int s, c;

    /* stdin and out are also FILE* */
    intfilesum(stdin, &s, &c);
    printf("Sum: %d, count: %d\n", s, c);

    return 0;
}
