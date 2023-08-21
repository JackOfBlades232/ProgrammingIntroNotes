/* recursion/match_rec.c */
#include <stdio.h>

int star_match(const char *str, const char *pat);
int match(const char *str, const char *pat)
{
    switch (*pat) {
        case '\0':
            return *str == 0;

        case '*':
            return star_match(str, pat+1);

        default:
            if (!*str || (*str != *pat && *pat != '?'))
                return 0;
            return match(str+1, pat+1);
    }
}

int star_match(const char *str, const char *pat)
{
    if (!*str)
        return match("", pat);
    if (match(str, pat))
        return 1;
    return star_match(str+1, pat);
}

void strip_nl(char *str)
{
    if (!*str)
        return;
    if (*str == '\n') {
        *str = '\0';
        return;
    }
    return strip_nl(str+1);
}

int main()
{
    char str[128],
         pat[128];

    printf("Input pattern: ");
    fgets(pat, sizeof(pat), stdin);
    printf("Input string: ");
    fgets(str, sizeof(str), stdin);

    strip_nl(str);
    strip_nl(pat);

    printf("Pattern: %s, String: %s, Result: %s\n", pat, str,
           match(str, pat) ? "matched" : "not matched");

    return 0;
}
