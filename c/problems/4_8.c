/* 4_8.c */
#include <stdio.h>

int count_spaces(const char *str)
{
    return *str ? count_spaces(str + 1) + (*str == ' ' ? 1 : 0) : 0;
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
