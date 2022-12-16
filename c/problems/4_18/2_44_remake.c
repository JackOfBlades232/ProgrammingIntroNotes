/* 4_18/2_44_remake.c */
#include <stdio.h>
#include <stdlib.h>

typedef struct tag_letter {
    char c;
    struct tag_letter *next;
} letter;

typedef struct tag_word {
    letter *first;
    int len;
    struct tag_word *next;
} word;

int char_is_digit(int c)
{
    return c >= '0' && c <= '9';
}

int char_is_eoln(int c)
{
    return c == EOF || c == '\n';
}

word *read_word_from_stdin(int *break_char, int *max_wrd_len)
{
    word *wrd = NULL;
    letter *last = NULL, *tmp;
    int c;

    while (!char_is_eoln(c = getchar())) {
        if (char_is_digit(c)) {
            tmp = malloc(sizeof(letter));
            tmp->c = c;
            tmp->next = NULL;

            if (!last)
                last = tmp;
            else {
                last->next = tmp;
                last = last->next;
            }

            if (!wrd) {
                wrd = malloc(sizeof(word));
                wrd->first = last;
                wrd->len = 1;
                wrd->next = NULL;
            } else
                (wrd->len)++;

        } else if (wrd)
            break;
    }

    *break_char = c;

    if (wrd && wrd->len > *max_wrd_len)
        *max_wrd_len = wrd->len;

    return wrd;
}

void print_and_delete_word(word *wrd)
{
    letter *last, *tmp;

    for (last = wrd->first; last;) {
        putchar(last->c);

        tmp = last;
        last = last->next;
        free(tmp);
    }

    putchar('\n');
    free(wrd);
}

void delete_word(word *wrd)
{
    letter *last, *tmp;

    for (last = wrd->first; last;) {
        tmp = last;
        last = last->next;
        free(tmp);
    }

    free(wrd);
}

int process_and_output_line()
{
    word *first = NULL, *last = NULL, *tmp;
    int break_char, max_wrd_len;

    break_char = 0;
    max_wrd_len = 0;
    while (!char_is_eoln(break_char)) {
        tmp = read_word_from_stdin(&break_char, &max_wrd_len);
        if (tmp) {
            if (!last)
                first = last = tmp;
            else {
                last->next = tmp;
                last = last->next;
            }
        }
    }

    while (first) {
        tmp = first;
        first = first->next;

        if (tmp->len == max_wrd_len)
            print_and_delete_word(tmp);
        else
            delete_word(tmp);            
    }

    return break_char;
}

int main()
{
    int break_char = 0;

    while ((break_char = process_and_output_line()) != EOF)
        {}

    return 0;
}
