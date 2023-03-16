/* 5_34.c */
#include <stddef.h>
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <string.h>

enum {
    del = 127
};

enum { 
    read_bufsize = 1024,
    output_bufsize = 4096,
    match_bufsize = 128 
};

void putchar_now(int c)
{
    putchar(c);
    fflush(stdout);
}

void putstr_now(const char *s)
{
    printf("%s", s);
    fflush(stdout);
}

int char_is_separator(int c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

int add_char_to_buf(char **bufpos, char *buf, size_t bufsize, char c)
{
    if (*bufpos - buf >= bufsize-1)
        return 0;

    **bufpos = c;
    (*bufpos)++;
    **bufpos = '\0';
    return 1;
}

int remove_char_from_buf(char **bufpos, char *buf, size_t bufsize)
{
    if (*bufpos - buf <= 0)
        return 0;

    (*bufpos)--;
    **bufpos = '\0';
    return 1;
}

void output_to_eol(FILE *f, char **bufpos, char *buf, size_t bufsize)
{
    int c;
    while ((c = fgetc(f)) != EOF) {
        if (c == '\n')
            break;

        if (bufpos != NULL) { /* remake condition */
            if (add_char_to_buf(bufpos, buf, bufsize, c)) 
                putchar_now(c);
        } else
            putchar_now(c);
    }
}

size_t look_up_word_by_prefix(FILE *dict, const char *prefix, size_t prefix_len,
        long (*match_positions)[match_bufsize], long *last_match_end)
{
    int c;
    int line_may_match;
    const char *prefix_p = prefix;

    size_t num_matches = 0;
   
    fseek(dict, 0, SEEK_SET);

    line_may_match = 1;
    **match_positions = ftell(dict);
    *last_match_end = -1;
    while ((c = fgetc(dict)) != EOF) {
        if (line_may_match && prefix_p-prefix >= prefix_len) {
            if (num_matches < match_bufsize-1) {
                *last_match_end = ftell(dict)-1;
                num_matches++;
            } else
                return -1;
            
            line_may_match = 0;
        } 

        if (c == '\n') {
            prefix_p = prefix;
            line_may_match = 1;
            (*match_positions)[num_matches] = ftell(dict);
        } else if (line_may_match) {
            if (c == *prefix_p)
                prefix_p++;
            else
                line_may_match = 0;
        }
    }

    return num_matches;
}

void complete_word(long dict_pos, FILE *dict_f, 
        char **bufpos, char *buf, size_t bufsize)
{
    fseek(dict_f, dict_pos, SEEK_SET);
    output_to_eol(dict_f, bufpos, buf, bufsize);
}

void output_multiple_matches(long *matches, size_t match_cnt, 
        FILE *dict_f, char *buf)
{
    int i;
    putchar_now('\n');
    for (i = 0; i < match_cnt; i++) {
        fseek(dict_f, matches[i], SEEK_SET);
        output_to_eol(dict_f, NULL, NULL, 0);
        putchar_now(' ');
    }
    putchar_now('\n');
    putstr_now(buf);
}

void perform_lookup(char **bufpos, char *buf, size_t bufsize, FILE *dict_f)
{
    char *prefix = *bufpos;
    size_t prefix_len;

    size_t match_cnt;
    long match_positions[match_bufsize];
    long last_match_end;

    while (prefix-buf > 0) {
        prefix--;
        if (char_is_separator(*prefix)) {
            prefix++;
            break;
        }
    }

    prefix_len = *bufpos - prefix;
    match_cnt = look_up_word_by_prefix(
            dict_f, prefix, prefix_len,
            &match_positions, &last_match_end
            );

    if (match_cnt == -1)
        putstr_now("\nToo many options to display\n");
    else if (match_cnt == 1)
        complete_word(last_match_end, dict_f, bufpos, buf, bufsize);
    else if (match_cnt > 1)
        output_multiple_matches(match_positions, match_cnt, dict_f, buf);
}

void process_backspace(char **bufpos, char *buf, size_t bufsize)
{
    if (remove_char_from_buf(bufpos, buf, bufsize))
        putstr_now("\b \b");
}

/* test: 1 line */
int parse_input(FILE *out_f, FILE *dict_f)
{
    char read_buf[read_bufsize];
    char out_f_buf[output_bufsize];

    int read_res;
    char *r_bufp, *f_bufp;
    
    f_bufp = out_f_buf;
    while ((read_res = read(0, read_buf, sizeof(read_buf))) > 0) {
        for (r_bufp = read_buf; r_bufp-read_buf < read_res; r_bufp++) {
            switch (*r_bufp) {
                case '\4':
                    if (f_bufp == out_f_buf)
                        goto end_of_file;
                    break;
                case '\n':
                    /* flush line and reset cur buf */
                    fprintf(out_f, "%s\n", out_f_buf);
                    f_bufp = out_f_buf;
                    putchar_now('\n');
                    break;
                case '\t':
                    /* look up current word prefix (check from out buf) */
                    perform_lookup(&f_bufp, out_f_buf,
                            sizeof(out_f_buf), dict_f);
                    break;
                case '\b':
                    /* remove from out buf, "\b \b" to screen */
                    process_backspace(&f_bufp, out_f_buf, sizeof(out_f_buf));
                    break;
                case del:
                    /* same as \b */
                    process_backspace(&f_bufp, out_f_buf, sizeof(out_f_buf));
                    break;
                default:
                    /* put to out buf and to screen */
                    if (add_char_to_buf(&f_bufp, out_f_buf, 
                                sizeof(out_f_buf), *r_bufp)) {
                        putchar_now(*r_bufp);
                    }
                    break;
            }
        }
    }

end_of_file:
    return 0;
}

int main(int argc, char **argv)
{
    int exit_code;
    struct termios ts1, ts2;
    FILE *out_f, *dict_f;

    if (!isatty(0)) {
        fprintf(stderr, "Not a terminal\n");
        return 1;
    }

    if (argc < 3) {
        fprintf(stderr, "Specify out file and dict file\n");
        return 2;
    }

    out_f = fopen(argv[1], "w");
    if (!out_f) {
        fprintf(stderr, "Could not open out file\n");
        return 3;
    }
    dict_f = fopen(argv[2], "r");
    if (!dict_f) {
        fprintf(stderr, "Could not open dict file\n");
        fclose(out_f);
        return 4;
    }

    tcgetattr(0, &ts1); 
    memcpy(&ts2, &ts1, sizeof(ts1)); 

    ts1.c_lflag &= ~(ICANON | IEXTEN | ECHO);
    ts1.c_lflag |= ISIG;
    ts1.c_cc[VMIN] = 1;
    ts1.c_cc[VTIME] = 0;
    ts1.c_cc[VEOF] = 0; /* CTRL_D seems not to work */

    tcsetattr(0, TCSANOW, &ts1); 

    exit_code = parse_input(out_f, dict_f);

    tcsetattr(0, TCSANOW, &ts2); 
    fclose(dict_f);
    fclose(out_f);
    return exit_code;
}
