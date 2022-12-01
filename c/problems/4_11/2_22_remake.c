/* 4_11/2_22_remake.c */
#include <stdio.h>

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
    static int char_counts[128];
    int i;
    const char *p;

    for (i = 0; i < 128; i++)
        char_counts[i] = 0;
    for (p = s; *p; p++)
        char_counts[(int)(*p)]++;
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
        default:
            fprintf(stderr, "Invalid subroblem letter! Must be a-g\n");
            return 1;
    }

    return 0;
}
