/* 9_6.c */
#include <stdio.h>

int match(const char *str, const char *pref)
{
    if (!*pref)
        return 1;
    if (!*str || *str != *pref)
        return 0;
    return match(str+1, pref+1);
}

char *strstr(const char *haystack, const char *needle)
{
    if (!*haystack)
        return NULL;
    return match(haystack, needle) ? (char *)haystack : strstr(haystack+1, needle);
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
    char haystack[128];
    char needle[128];

    printf("Input haystack: ");
    fgets(haystack, sizeof(haystack), stdin);
    printf("Input needle: ");
    fgets(needle, sizeof(needle), stdin);

    strip_nl(haystack);
    strip_nl(needle);

    printf("Spot (with additional funcs): %s\n", strstr(haystack, needle));
    /* TODO: implement variation with one func */

    return 0;
}
