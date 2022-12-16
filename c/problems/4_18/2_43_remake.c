/* 4_18/2_43_remake.c */
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
                wrd->next = NULL;
            }

        } else if (wrd)
            break;
    }

    *break_char = c;

    return wrd;
}

int print_and_delete_line(word *f_wrd)
{
    letter *tmp;
    int not_last_line;

    not_last_line = 0;
    for (; f_wrd; f_wrd = f_wrd->next) {
        if (f_wrd->first) {
            tmp = f_wrd->first;
            putchar(tmp->c);
            f_wrd->first = tmp->next;
            free(tmp);

            if (!not_last_line && f_wrd->first)
                not_last_line = 1;
        } else 
            putchar(' ');
    }

    putchar('\n');

    return not_last_line;
}

void delete_words(word *f_wrd)
{
    word *tmp;
    
    while (f_wrd) {
        tmp = f_wrd;
        f_wrd = f_wrd->next;
        free(tmp);
    }
}

int process_and_output_line()
{
    word *first = NULL, *last = NULL, *tmp;
    int break_char;

    break_char = 0;
    while (!char_is_eoln(break_char)) {
        tmp = read_word_from_stdin(&break_char);
        if (tmp) {
            if (!last)
                first = last = tmp;
            else {
                last->next = tmp;
                last = last->next;
            }
        }
    }

    while (print_and_delete_line(first))
        {}

    delete_words(first);

    return break_char;
}


int main()
{
    int break_char = 0;

    while ((break_char = process_and_output_line()) != EOF)
        {}

    return 0;
}
