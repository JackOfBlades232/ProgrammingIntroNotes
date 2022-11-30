/* 4_7.c */
#include <stdio.h>

int count_spaces(const char *str)
{
    const char *ptr;
    int cnt;

    cnt = 0;
    for (ptr = str; *ptr; ptr++) {
        if (*ptr == ' ')
            cnt++;
    }

    return cnt;
}

int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stderr, "Input 1 cli arg");
        return 1;
    }

    printf("Num spaces: %d\n", count_spaces(argv[1]));

    return 0;
}
