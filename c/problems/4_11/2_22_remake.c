/* 4_11/2_22_remake.c */
#include <stdio.h>
#include <stdlib.h>

static int char_counts[128];

/* general */
static int string_len(const char *s)
{
    const char *p;
    for (p = s; *p; p++)
        {}
    return p - s;
}

void print_args_by_predicate(int argc, char **argv, 
        int (*predicate_ptr) (const char *))
{
    int i;
    for (i = 1; i < argc; i++)
        if ((*predicate_ptr)(argv[i]))
            printf("%s ", argv[i]);

    putchar('\n');
}

void fill_char_counts(const char *s)
{
    int i;
    for (i = 0; i < 128; i++)
        char_counts[i] = 0;
    for (; *s; s++)
        char_counts[(int)(*s)]++;
}

/* a) */
void print_longest_arg(int argc, char **argv)
{
    int i, len, max_len, max_len_idx;

    max_len = 0;
    max_len_idx = 0;
    for (i = 1; i < argc; i++) {
        len = string_len(argv[i]);

        if (max_len < len) {
            max_len = len;
            max_len_idx = i;
        }
    }

    if (max_len_idx > 0)
        printf("%s\n", argv[max_len_idx]);
} 

/* b) */
int string_has_no_repeating_chars(const char *s)
{
    int i;

    fill_char_counts(s);
    for (i = 0; i < 128; i++)
        if (char_counts[i] > 1)
            return 0;

    return 1;
}

/* c) */
int string_has_one_dog_and_at_lst_one_dot(const char *s)
{
    int dot_cnt, dog_cnt;

    dot_cnt = dog_cnt = 0;
    for (; *s; s++) {
        if (*s == '.')
            dot_cnt++;
        else if (*s == '@')
            dog_cnt++;

        if (dog_cnt > 1)
            return 0;
    }

    return dot_cnt >= 1 && dog_cnt == 1;
}

/* d) */
int string_is_numeric(const char *s)
{
    for (; *s; s++)
        if (*s < '0' || *s > '9')
            return 0;

    return 1;
}

/* e) */
int string_is_same_char(const char *s)
{
    char c;

    if (!*s)
        return 1;

    c = *s;
    for (s++; *s; s++)
        if (c != *s)
            return 0;

    return 1;
}

/* f) */
int char_is_letter(char c)
{
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

int string_has_letters(const char *s)
{
    for (; *s; s++)
        if (char_is_letter(*s))
            return 1;

    return 0;
}

/* g) */
int string_has_letters_from_char_count(const char *s)
{
    for (; *s; s++)
        if (char_counts[(int)(*s)] > 0)
            return 1;

    return 0;
}

void print_args_with_same_letters_as_first(int argc, char **argv)
{
    int i;
    
    if (argc < 2)
    {
        fprintf(stderr, "Provide template arg!\n");
        exit(1);
    }

    fill_char_counts(argv[1]);
    for (i = 2; i < argc; i++)
        if (string_has_letters_from_char_count(argv[i]))
            printf("%s ", argv[i]);

    putchar('\n');
}
        

int main(int argc, char **argv)
{
    char subp;

    printf("Input subploblem letter: ");
    scanf("%c", &subp);
    
    switch (subp) {
        case 'a':
            print_longest_arg(argc, argv);
            break;
        case 'b':
            print_args_by_predicate(argc, argv, string_has_no_repeating_chars);
            break;
        case 'c':
            print_args_by_predicate(argc, argv,
                    string_has_one_dog_and_at_lst_one_dot);
            break;
        case 'd':
            print_args_by_predicate(argc, argv, string_is_numeric);
            break;
        case 'e':
            print_args_by_predicate(argc, argv, string_is_same_char);
            break;
        case 'f':
            print_args_by_predicate(argc, argv, string_has_letters);
            break;
        case 'g':
            print_args_with_same_letters_as_first(argc, argv);
            break;
        default:
            fprintf(stderr, "Invalid subroblem letter! Must be a-g\n");
            return 1;
    }

    return 0;
}
