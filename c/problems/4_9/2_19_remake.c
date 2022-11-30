/* 4_9/2_19_remake.c */
#include <stdio.h>

/* general functions */
static void update_max_val(int new_val, int *max_val)
{
    if (new_val > *max_val)
        *max_val = new_val;
}

static void update_min_val(int new_val, int *min_val)
{
    if (new_val > *min_val)
        *min_val = new_val;
}

static int char_is_space(char c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

static int is_end_of_word(char c, char prev_c)
{
    return char_is_space(c) && !char_is_space(prev_c);
}

static int is_start_of_word(char c, char prev_c)
{
    return !char_is_space(c) && char_is_space(prev_c);
}

void count_words_of_two_types(int *odd_cnt, int *even_cnt,
        void (*choose_inc_cnt_ptr) (int, int *, int *))
{
    char c, prev_c;
    int word_len;

    *odd_cnt = 0;
    *even_cnt = 0;
    word_len = 0;
    prev_c = ' ';
    while ((c = getchar()) != EOF) {
        if (is_end_of_word(c, prev_c)) {
            (*choose_inc_cnt_ptr)(word_len, odd_cnt, even_cnt);
            word_len = 0;
        }
        else if (!char_is_space(c))
            word_len++;
            
        prev_c = c;
    }

    if (!char_is_space(prev_c)) 
        (*choose_inc_cnt_ptr)(word_len, odd_cnt, even_cnt);
}

/* a) */
int count_words()
{
    char c, prev_c;
    int cnt;

    cnt = 0;
    prev_c = ' ';
    while ((c = getchar()) != EOF) {
        if (is_end_of_word(c, prev_c))
            cnt++;
        prev_c = c;
    }

    return cnt + (char_is_space(prev_c) ? 0 : 1);
}

/* b) */
void choose_inc_cnt_oe(int word_len, int *odd_cnt, int *even_cnt)
{
    if (word_len % 2)
        (*odd_cnt)++;
    else
        (*even_cnt)++;
}

/* c) */
void choose_inc_cnt_bounded(int word_len, int *more_than_seven_cnt, 
        int *less_than_three_cnt)
{
    if (word_len > 7)
        (*more_than_seven_cnt)++;
    else if (word_len < 3)
        (*less_than_three_cnt)++;
}

/* d) */
int word_ended_and_az(char c, char prev_c, char start_c)
{
    return is_end_of_word(c, prev_c) && start_c == 'A' && prev_c == 'z';
}

int final_word_ended_and_az(char c, char prev_c, char start_c)
{
    return (!char_is_space(prev_c) && word_ended_and_az(' ', prev_c, start_c));
}

int count_words_az()
{
    char c, prev_c, start_c;
    int cnt;

    cnt = 0;
    prev_c = ' ';
    while ((c = getchar()) != EOF) {
        if (is_start_of_word(c, prev_c))
            start_c = c;
        else if (word_ended_and_az(c, prev_c, start_c))
            cnt++;
        prev_c = c;
    }

    return cnt + (final_word_ended_and_az(c, prev_c, start_c) ? 1 : 0);
}

int main()
{
    char c;
    int f_cnt, s_cnt;

    printf("Input subproblem letter: ");
    scanf("%c", &c);

    switch (c) {
        case 'a':
            printf("Num words: %d\n", count_words());
            break;
        case 'b':
            count_words_of_two_types(&f_cnt, &s_cnt, choose_inc_cnt_oe);
            printf("Num odd words: %d, num even words: %d\n", f_cnt, s_cnt);
            break;
        case 'c':
            count_words_of_two_types(&f_cnt, &s_cnt, choose_inc_cnt_bounded);
            printf("Num long words: %d, num short words: %d\n", f_cnt, s_cnt);
            break;
        case 'd':
            printf("Num words starting with 'A' and ending with 'z': %d\n",
                    count_words_az());
            break;
        default:
            fprintf(stderr, "Invalid subproblem letter, input a-h\n");
            return 1;
    }

    return 0;
}
