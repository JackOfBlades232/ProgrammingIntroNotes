/* 5_7/strings.c */
#include "strings.h"

int strings_are_equal(const char *s1, const char *s2)
{
    for (; *s1 && *s2; s1++, s2++) {
        if (*s1 != *s2)
            return 0;
    }

    return *s1 == *s2;
}

int string_copy(const char *src, char *dest, int len)
{
    int i;
    for (i = 0; i < len - 1; i++) {
        *dest = *src;

        if (*src == '\0')
            return i+1;
        
        src++;
        dest++;
    }

    *dest = '\0';
    return i+1;
}
