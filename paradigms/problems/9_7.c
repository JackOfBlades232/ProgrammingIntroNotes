/* 9_7.c */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Non recursive naive wildcard pattern matching
 */

#define NOOP

struct star_frame {
    int str_idx, pat_idx;
};

int match(const char *str, const char *pat)
{
    int str_len = strlen(str);
    int pat_len = strlen(pat);

    struct star_frame *star_stack = malloc(str_len * sizeof(*star_stack));

    for (int i = 0; i <= str_len-pat_len; i++) {
        int s_idx = i;
        int p_idx = 0;

        int stack_head = -1;

        while (s_idx < str_len && p_idx < pat_len) {
            if (pat[p_idx] == '*') {
                p_idx++;
                stack_head++;
                star_stack[stack_head].str_idx = s_idx;
                star_stack[stack_head].pat_idx = p_idx;
            } else if (str[s_idx] != pat[p_idx] && pat[p_idx] != '?') {
                while (str_len - s_idx < pat_len - p_idx) {
                    if (stack_head < 0)
                        goto inner_loop_break;

                    star_stack[stack_head].str_idx++;
                    s_idx = star_stack[stack_head].str_idx;
                    p_idx = star_stack[stack_head].pat_idx;
                    stack_head--;
                }

                stack_head++;
            } else {
                s_idx++;
                p_idx++;
            }
        }

inner_loop_break:
        NOOP;

        if (p_idx == pat_len)
            return 1;
    }

    return 0;
}

void strip_nl(char *str)
{
    if (!*str)
        return;
    if (*str == '\n') {
        *str = '\0';
        return;
    }
    return strip_nl(str+1);
}

int main()
{
    char str[128],
         pat[128];

    printf("Input pattern: ");
    fgets(pat, sizeof(pat), stdin);
    printf("Input string: ");
    fgets(str, sizeof(str), stdin);

    strip_nl(str);
    strip_nl(pat);

    printf("Pattern: %s, String: %s, Result: %s\n", pat, str,
           match(str, pat) ? "matched" : "not matched");

    return 0;
}
