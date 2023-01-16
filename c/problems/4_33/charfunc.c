/* 4_33/charfunc.c */
#ifndef CHARFUNC_SENTRY
#define CHARFUNC_SENTRY

int char_is_digit(char c)
{
    return c >= '0' && c <= '9';
}

int char_is_arifm_sign(char c)
{
    return c == '+' || c == '-' || c == '*' || c == '/';
}

int char_is_sign(char c)
{
    return c == '(' || c == ')' || char_is_arifm_sign(c);
}

int char_is_stringbreak(char c)
{
    return c == '\0' || c == '\n' || c == '\r';
}

#endif
