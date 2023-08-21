/* 9_7.c */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Non recursive naive wildcard pattern matching
 */

void remove_consecutive_stars(char *str)
{
    int len = strlen(str);

    for (int i = 0; i < len-1; i++) {
        int offset = 0;
        while (str[i+offset] == '*' && str[i+offset+1] == '*')
            offset++;
        for (int j = 0; j < len - i - offset + 1; j++)
            str[i+j] = str[i+j+offset];
        len -= offset;
    }
}

struct star_frame {
    int str_idx, pat_idx;
};

int match(const char *str, const char *ptn)
{
    char *pat = strdup(ptn);
    remove_consecutive_stars(pat);

    int str_len = strlen(str);
    int pat_len = strlen(pat);

    int stars_left = 0;
    for (int i = 0; i < pat_len; i++) {
        if (pat[i] == '*')
            stars_left++;
    }

    struct star_frame *star_stack = malloc(stars_left * sizeof(*star_stack));
    int stack_head = -1;

    int s_idx = 0,
        p_idx = 0;

    for (;;) {
        if (p_idx < pat_len && pat[p_idx] == '*') {
            p_idx++;
            stack_head++;
            stars_left--;
            star_stack[stack_head].str_idx = s_idx;
            star_stack[stack_head].pat_idx = p_idx;
        } else if (s_idx >= str_len)
            break;
        else if (
                p_idx >= pat_len || 
                (str[s_idx] != pat[p_idx] && pat[p_idx] != '?')
                )
        {
            if (stack_head < 0)
                break;
            star_stack[stack_head].str_idx++;
            s_idx = star_stack[stack_head].str_idx;
            p_idx = star_stack[stack_head].pat_idx;

            while (str_len - s_idx < pat_len - p_idx - stars_left) {
                stack_head--;
                stars_left++;
                if (stack_head < 0)
                    goto loop_break;

                star_stack[stack_head].str_idx++;
                s_idx = star_stack[stack_head].str_idx;
                p_idx = star_stack[stack_head].pat_idx;
            }
        } else {
            s_idx++;
            p_idx++;
        }
    }

loop_break:
    return p_idx >= pat_len &&
        (s_idx >= str_len || (p_idx > 0 && pat[p_idx-1] == '*'));
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
