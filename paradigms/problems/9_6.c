/* 9_6.c */
#include <stdio.h>

int strlen_rec(const char *str)
{
    if (!*str)
        return 0;
    return strlen_rec(str+1) + 1;
}

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

char *strstr_bare(const char *haystack, int haystack_len, 
                  const char *needle, int needle_len)
{
    if (needle_len <= 0)
        return (char *)haystack;
    if (haystack_len <= 0)
        return NULL;
    if (
            *haystack == *needle &&
            strstr_bare(haystack+1, needle_len-1, needle+1, needle_len-1)
       )
    {
        return (char *)haystack;
    } else
        return strstr_bare(haystack+1, haystack_len-1, needle, needle_len);
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
    printf("Spot (with one func): %s\n",
           strstr_bare(haystack, strlen_rec(haystack),
                       needle, strlen_rec(needle)));

    return 0;
}
