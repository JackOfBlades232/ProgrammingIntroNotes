/* 5_34.c */
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <string.h>

#define CTRL_D '\4'
#define DEL 127

enum { bufsize = 256 };
enum { match_cap = 128 };

int add_char_to_buf(char **bufpos, char *buf, char c)
{
    if (*bufpos - buf >= bufsize-1)
        return 0;

    **bufpos = c;
    (*bufpos)++;
    **bufpos = '\0';
    return 1;
}

int remove_char_from_buf(char **bufpos, char *buf)
{
    if (*bufpos == buf)
        return 0;

    (*bufpos)--;
    **bufpos = '\0';
    return 1;
}

int char_is_inword(char c)
{
    return c != ' ' && c != '\t' && c != '\n' && c != '\r';
}

int output_to_eol(FILE *f, char **bufpos, char *buf)
{
    int c;
    while ((c = fgetc(f)) != EOF) {
        if (c == '\n')
            break;
        putchar(c);

        if (bufpos != NULL && buf != NULL) {
            if (!add_char_to_buf(bufpos, buf, c))
                return 0;
        }
    }

    return 1;
}

int look_up_word_by_prefix(FILE *dict, const char *prefix,
        long (*match_positions)[match_cap], long *last_match_end)
{
    int c;
    int line_may_match;
    const char *prefix_p = prefix;

    int num_matches = 0;
   
    fseek(dict, 0, SEEK_SET);

    line_may_match = 1;
    **match_positions = ftell(dict);
    *last_match_end = -1;
    while ((c = fgetc(dict)) != EOF) {
        if (line_may_match && *prefix_p == '\0') {
            if (num_matches < match_cap-1) {
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

int perform_lookup(char **bufpos, char *buf, char *cur_word, FILE *dict_f)
{
    long match_positions[match_cap];
    long last_match_end;
    int i;

    int num_matches = look_up_word_by_prefix(dict_f, cur_word,
            &match_positions, &last_match_end);
    
    if (num_matches > 1) {
        putchar('\n');
        for (i = 0; i < num_matches; i++) {
            fseek(dict_f, match_positions[i], SEEK_SET);
            output_to_eol(dict_f, NULL, NULL);
            putchar(' ');
        }
        putchar('\n');
        printf("%s", buf);
        return 1;
    } 

    if (num_matches == 1) {
        fseek(dict_f, last_match_end, SEEK_SET);
        return output_to_eol(dict_f, bufpos, buf);
    }

    return num_matches == 0;
}

int process_input(FILE *out_f, FILE *dict_f)
{
    int c;

    char buf[bufsize];
    char *bufp, *cur_word;

    bufp = buf;
    cur_word = NULL;
    *bufp = '\0';

    while ((c = getchar()) != EOF) {
        if (c == CTRL_D && buf == bufp) /* eof emulation */
            break;
        else if (c == '\b' || c == DEL) { /* backspace/del */
            if (remove_char_from_buf(&bufp, buf))
                printf("\b \b");

            continue;
        }
            
        if (cur_word != NULL) {
            if (c == '\t') {
                if (perform_lookup(&bufp, buf, cur_word, dict_f))
                    continue;
                else
                    return 0;
            }

            if (c == ' ')
                cur_word = NULL;
        } else if (char_is_inword(c))
            cur_word = bufp;

        putchar(c);
        add_char_to_buf(&bufp, buf, c);

        if (c == '\n') {
            fputs(buf, out_f);
            bufp = buf;
            cur_word = NULL;
            *bufp = '\0';
        }
    }

    return 1;
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
    ts1.c_lflag &= ~(ICANON | ECHO);
    ts1.c_cc[VMIN] = 1;
    ts1.c_cc[VTIME] = 0;
    ts1.c_cc[VEOF] = 0;
    ts1.c_cc[VERASE] = 0;
    tcsetattr(0, TCSANOW, &ts1); 

    exit_code = process_input(out_f, dict_f) ? 0 : 5;
    if (exit_code != 0)
        fprintf(stderr, "Line/match cap exceeded\n");

    tcsetattr(0, TCSANOW, &ts2); 
    fclose(dict_f);
    fclose(out_f);
    return exit_code;
}
