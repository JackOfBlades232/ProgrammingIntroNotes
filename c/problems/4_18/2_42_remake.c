/* 4_18/2_42_remake.c */
#include <stdio.h>
#include <stdlib.h>

typedef struct tag_letter {
    char c;
    struct tag_letter *next;
} letter;

typedef struct tag_word {
    letter *first;
    struct tag_word *next;
} word;

int char_is_space(int c)
{
    return c == ' ' || c == '\t' || c == '\r';
}

int char_is_eoln(int c)
{
    return c == EOF || c == '\n';
}

word *read_word_from_stdin(int *break_char)
{
    word *wrd = NULL;
    letter *last = NULL, *tmp;
    int c;

    while (!char_is_eoln(c = getchar())) {
        if (!char_is_space(c)) {
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
            }

        } else if (wrd)
            break;
    }

    *break_char = c;

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

    putchar(' ');
    free(wrd);
}

int process_and_reverse_line()
{
    word *first = NULL, *tmp;
    int break_char;

    break_char = 0;
    while (!char_is_eoln(break_char)) {
        tmp = read_word_from_stdin(&break_char);
        if (tmp) {
            tmp->next = first;
            first = tmp;
        }
    }

    while (first) {
        tmp = first;
        first = first->next;
        
        print_and_delete_word(tmp); 
    }

    putchar('\n');

    return break_char;
}

int main()
{
    int break_char = 0;

    while ((break_char = process_and_reverse_line()) != EOF)
        {}

    return 0;
}
