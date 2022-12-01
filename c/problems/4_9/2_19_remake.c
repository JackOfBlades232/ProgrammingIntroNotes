/* 4_9/2_19_remake.c */
#include <stdio.h>
#include <limits.h>

/* general functions */
static void update_max_val(int new_val, int *max_val)
{
    if (new_val > *max_val)
        *max_val = new_val;
}

static void update_min_val(int new_val, int *min_val)
{
    if (new_val < *min_val)
        *min_val = new_val;
}

static int char_is_wordbreak(char c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

static int is_end_of_word(char c, char prev_c)
{
    return char_is_wordbreak(c) && !char_is_wordbreak(prev_c);
}

static int is_start_of_word(char c, char prev_c)
{
    return !char_is_wordbreak(c) && char_is_wordbreak(prev_c);
}

void count_words_of_two_types(int *f_cnt, int *s_cnt,
        void (*choose_inc_cnt_ptr) (int, int *, int *))
{
    char c, prev_c;
    int word_len;

    *f_cnt = 0;
    *s_cnt = 0;
    word_len = 0;
    prev_c = ' ';
    while ((c = getchar()) != EOF) {
        if (is_end_of_word(c, prev_c)) {
            (*choose_inc_cnt_ptr)(word_len, f_cnt, s_cnt);
            word_len = 0;
        }
        else if (!char_is_wordbreak(c))
            word_len++;
            
        prev_c = c;
    }

    if (!char_is_wordbreak(prev_c)) 
        (*choose_inc_cnt_ptr)(word_len, f_cnt, s_cnt);
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

    return cnt + (char_is_wordbreak(prev_c) ? 0 : 1);
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
static int word_ended_and_az(char c, char prev_c, char start_c)
{
    return is_end_of_word(c, prev_c) && start_c == 'A' && prev_c == 'z';
}

static int final_word_ended_and_az(char c, char prev_c, char start_c)
{
    return !char_is_wordbreak(prev_c) &&
        word_ended_and_az(' ', prev_c, start_c);
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

/* e) */
static void update_min_max_cnt(int *word_len, int *cnt, int *min, int *max)
{
    update_min_val(*word_len, min);
    update_max_val(*word_len, max);
    (*cnt)++;
    *word_len = 0;
}

void count_min_max(int *cnt, int *min, int *max)
{
    char c, prev_c;
    int word_len;

    *cnt = 0;
    *min = INT_MAX;
    *max = 0;
    word_len = 0;
    prev_c = ' ';
    while ((c = getchar()) != EOF) {
        if (is_end_of_word(c, prev_c)) 
            update_min_max_cnt(&word_len, cnt, min, max);
        else if (!char_is_wordbreak(c))
            word_len++;
            
        prev_c = c;
    }

    if (!char_is_wordbreak(prev_c)) 
        update_min_max_cnt(&word_len, cnt, min, max);
}

/* f) */
static int is_end_of_space_seq(char c, char prev_c)
{
    return c != ' ' && prev_c == ' ';
}

void count_max_word_and_space_seq(int *m_wrd, int *m_spc)
{
    char c, prev_c;
    int word_len, spaces_len;

    *m_wrd = 0;
    *m_spc = 0;
    word_len = 0;
    spaces_len = 0;
    prev_c = ' ';
    while ((c = getchar()) != EOF) {
        if (is_end_of_word(c, prev_c)) {
            update_max_val(word_len, m_wrd);
            word_len = 0;
        }
        else if (!char_is_wordbreak(c))
            word_len++;
        
        if (is_end_of_space_seq(c, prev_c)) {
            update_max_val(spaces_len, m_spc);
            spaces_len = 0;
        }
        else if (c == ' ')
            spaces_len++;
            
        prev_c = c;
    }

    if (!char_is_wordbreak(prev_c)) 
        update_max_val(word_len, m_wrd);
    else if (prev_c == ' ')
        update_max_val(spaces_len, m_spc);
}

/* g) */
int check_bracket_balance()
{
    char c;
    int balance;

    balance = 0;
    while ((c = getchar()) != EOF) {
        if (c == '(')
            balance++;
        else if (c == ')')
            balance--;

        if (balance < 0)
            return 0;
    }

    return balance == 0;
}

/* h) */
int count_bracket_pairs()
{
    int cnt;
    char c, prev_c;

    prev_c = '\0';
    while ((c = getchar()) != EOF) {
        if (c == ')' && prev_c == '(')
            cnt++;
        prev_c = c;
    }

    return cnt;
}


int main()
{
    char c;
    int cnt, f_cnt, s_cnt;

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
        case 'e':
            count_min_max(&cnt, &f_cnt, &s_cnt);
            printf("Num words: %d, shortest wrod len: %d, "
                    "longest word len: %d\n", cnt, f_cnt, s_cnt);
            break;
        case 'f':
            count_max_word_and_space_seq(&f_cnt, &s_cnt);
            printf("Max word len: %d, Max space seq len: %d\n", f_cnt, s_cnt);
            break;
        case 'g':
            printf(check_bracket_balance() ? "YES\n" : "NO\n");
            break;
        case 'h':
            printf("Num bracket pairs: %d\n", count_bracket_pairs());
            break;
        default:
            fprintf(stderr, "Invalid subproblem letter, input a-h\n");
            return 1;
    }

    return 0;
}
