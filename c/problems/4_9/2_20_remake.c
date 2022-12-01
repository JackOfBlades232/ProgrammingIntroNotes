/* 4_9/2_20_remake.c */
#include <stdio.h>

static int char_is_wordbreak(char c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

static int is_start_of_word(char c, char prev_c)
{
    return !char_is_wordbreak(c) && char_is_wordbreak(prev_c);
}

static int is_end_of_word(char c, char prev_c)
{
    return char_is_wordbreak(c) && !char_is_wordbreak(prev_c);
}

void print_stdin_words_with_brackets()
{
    char c, prev_c;
    
    prev_c = ' ';
    while ((c = getchar()) != EOF) {
        if (is_start_of_word(c, prev_c))
            putchar('(');
        if (!char_is_wordbreak(c))
            putchar(c);
        if (is_end_of_word(c, prev_c)) {
            putchar(')');
            putchar(' ');
        }
        if (c == '\n')
            putchar(c);

        prev_c = c;
    }
}

int main()
{
    print_stdin_words_with_brackets();
    return 0;
}
