/* 4_9/2_21_remake.c */
#include <stdio.h>

static int char_is_wordbreak(char c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

static int is_end_of_word(char c, char prev_c)
{
    return char_is_wordbreak(c) && !char_is_wordbreak(prev_c);
}

void print_two_char_long_words_from_stdin()
{
    char c, prev_c, two_prev_c;
    int word_len;

    prev_c = ' ';
    two_prev_c = ' ';
    word_len = 0;
    while ((c = getchar()) != EOF) {
        if (is_end_of_word(c, prev_c)) {
            if (word_len == 2)
                printf("%c%c ", two_prev_c, prev_c);
            
            word_len = 0;
        }
        else if (!char_is_wordbreak(c))
            word_len++;

        if (c == '\n')
            putchar(c);

        two_prev_c = prev_c;
        prev_c = c;
    }
}

int main()
{
    print_two_char_long_words_from_stdin();
    return 0;
}
