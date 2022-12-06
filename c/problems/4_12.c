/* 4_12.c */
#include <stdio.h>

const char *search_pattern(const char *str, const char *pat)
{
    const char *s_cur, *p_cur;

    for (; *str; str++) {
        for (s_cur = str, p_cur = pat; *s_cur && *s_cur == *p_cur;
                s_cur++, p_cur++) {}
        if (!*p_cur)
            return str;
    }

    return NULL;
}

void print_strings_with_pattern(const char *pat, char **strs)
{
    for (; *strs; strs++)
        if (search_pattern(*strs, pat))
            printf("%s\n", *strs);
}

int count_pattern_occurances(const char *str, const char *pat)
{
    int count;

    count = 0;
    while ((str = search_pattern(str, pat))) {
        count++;
        str++;
    }

    return count;
}

void print_strings_with_pattern_and_counts(const char *pat, char **strs)
{
    int count;

    for (; *strs; strs++)
        if ((count = count_pattern_occurances(*strs, pat)))
            printf("%s %d\n", *strs, count);
}

int main(int argc, char **argv)
{
    char subproblem;

    if (argc < 3) {
        fprintf(stderr, "Provide pattern and at least one string as args!\n");
        return 1;
    }

    printf("Input subproblem letter (a/b): ");
    scanf("%c", &subproblem);
    switch (subproblem) {
        case 'a':
            print_strings_with_pattern(argv[1], argv + 2);
            break;
        case 'b':
            print_strings_with_pattern_and_counts(argv[1], argv + 2);
            break;
        default:
            fprintf(stderr, "Invalid subproblem letter!\n");
            return 2;
    }

    return 0;
}
