/* 4_33/intstr.c */
#include "charfunc.c"

int remove_spaces(char *str, int strlen)
{
    int i, reduction = 0;

    for (i = 0; i < strlen; i++) {
        if (str[i] == ' ' || str[i] == '\t')
            reduction++;
        else if (reduction > 0)
            str[i - reduction] = str[i];
    }

    return strlen - reduction;
}

int parse_int(const char *str, int strlen, int *out, char *brkchr)
{
    int chars_read = 1;

    *out = 0;
    for (; chars_read <= strlen; str++, chars_read++) {
        if (!char_is_digit(*str))
            break;

        *out *= 10;
        *out += *str - '0';
    }

    *brkchr = *str;
    return chars_read;
}

int stringify_int(int num, char *dest, int destlen)
{
    int status = 0;
    char *lp, *fp;

    lp = dest;
    for (; lp - dest < destlen; lp++) {
        *lp = num%10 + '0';
        num /= 10;
        if (num == 0) {
            status = 1;
            break;
        }
    }

    if (status == 0)
        return status;
    else
        status = lp - dest + 1;

    for (fp = dest; lp - fp > 0; fp++, lp--) {
        char tmp = *fp;
        *fp = *lp;
        *lp = tmp;
    }

    return status;
}
