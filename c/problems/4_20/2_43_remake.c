/* 4_20/2_43_remake.c */
#include <stdio.h>
#include <stdlib.h>

enum constants { min_str_mem = 16, str_mem_multiplier = 2 };

typedef struct tag_string {
    int mem;
    int len;
    char *content;
} string;

typedef struct tag_word {
    string str;
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

word *init_word()
{
    word *wrd;

    wrd = malloc(sizeof(word));
    (wrd->str).mem = min_str_mem;
    (wrd->str).content = calloc((wrd->str).mem, sizeof(char));
    wrd->next = NULL;

    return wrd;
}

void delete_word(word *wrd)
{
    free((wrd->str).content);
    free(wrd);
}

void expand_and_realloc_str(string *str)
{
    char *new_content, *p, *new_p;

    str->mem *= str_mem_multiplier;
    new_content = calloc(str->mem, sizeof(char));

    for (p = str->content, new_p = new_content; *p; p++, new_p++)
        *new_p = *p;

    free(str->content);
    str->content = new_content;
}

word *read_word_from_stdin(int *break_char)
{
    word *wrd = NULL;
    int c, idx;

    idx = 0;
    while (!char_is_eoln(c = getchar())) {
        if (!char_is_space(c)) {
            if (!wrd) 
                wrd = init_word();

            if (idx >= (wrd->str).mem - 1)
                expand_and_realloc_str(&(wrd->str));

            (wrd->str).content[idx] = c;
            idx++;
        } else if (wrd)
            break;
    }

    if (wrd)
        (wrd->str).len = idx;

    *break_char = c;

    return wrd;
}

int print_line(word *f_wrd, int idx)
{
    int not_last_line;

    not_last_line = 0;
    for (; f_wrd; f_wrd = f_wrd->next) {
        if ((f_wrd->str).len > idx) {
            putchar((f_wrd->str).content[idx]);

            if (!not_last_line && (f_wrd->str).len - 1 > idx )
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
        delete_word(tmp);
    }
}

int process_and_output_line()
{
    word *first = NULL, *last = NULL, *tmp;
    int break_char, i;

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

    for (i = 0; print_line(first, i); i++)
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
