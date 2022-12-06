/* 4_14.c */
#include <stdio.h>

int char_is_space(char c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

void remove_spaces(char *str)
{
    char *from, *to;

    for (from = to = str; *from; from++) {
        if (!char_is_space(*from)) {
            if (from > to)
                *to = *from;
            to++;
        }
    }

    if (from > to)
        *to = '\0';
}

int main(int argc, char **argv)
{
    int i;

    if (argc < 2) {
        fprintf(stderr, "Provide at least one cli argument!\n");
        return 1;
    }

    for (i = 1; i < argc; i++) {
        remove_spaces(argv[i]);
        printf("%s\n", argv[i]);
    }

    return 0;
}
