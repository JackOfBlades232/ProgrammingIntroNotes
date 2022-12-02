/* 4_11/2_23_remake.c */
#include <stdio.h>

int char_is_space(char c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

int words_in_string(const char *s)
{
    char prev;
    int cnt;

    for (prev = ' ', cnt = 0; *s; prev = *s, s++) 
        if (char_is_space(*s) && !char_is_space(prev))
            cnt++;

    return cnt + (char_is_space(prev) ? 0 : 1);
}

int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stderr, "Provide exactly 1 arg!\n");
        return 1;
    }

    printf("Words in arg: %d\n", words_in_string(argv[1]));
    return 0;
}
