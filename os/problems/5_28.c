/* 5_28.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>

#define D

#if defined(A)

enum {
    lines_to_show = 10
};

int filter_pipe(int fd)
{
    FILE *f;
    int c;
    int line_cnt = 0;

    f = fdopen(fd, "r");
    if (!f)
        return -1;

    while ((c = getc(f)) != EOF) {
        if (line_cnt < lines_to_show)
            putchar(c);

        if (c == '\n')
            line_cnt++;
    }

    return 0;
}

#elif defined(B)

enum line_type { uninited, valid, invalid };

int filter_pipe(int fd)
{
    FILE *f;
    int c;
    enum line_type l_type = uninited;

    f = fdopen(fd, "r");
    if (!f)
        return -1;

    while ((c = getc(f)) != EOF) {
        if (l_type == uninited)
            l_type = (c == ' ' || c == '\t') ? valid : invalid;

        if (l_type == valid)
            putchar(c);

        if (c == '\n')
            l_type = uninited;
    }

    return 0;
}

#elif defined(C)

enum {
    chars_to_show = 10
};

int filter_pipe(int fd)
{
    FILE *f;
    int c;
    int char_cnt = 0;

    f = fdopen(fd, "r");
    if (!f)
        return -1;

    while ((c = getc(f)) != EOF) {
        if (char_cnt < chars_to_show || c == '\n')
            putchar(c);

        if (c == '\n')
            char_cnt = 0;
        else
            char_cnt++;
    }

    return 0;
}

#else

int filter_pipe(int fd)
{
    FILE *f;
    int c;
    int reached_word = 0, printed_word = 0;

    f = fdopen(fd, "r");
    if (!f)
        return -1;

    while ((c = getc(f)) != EOF) {
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            if (reached_word && !printed_word)
                printed_word = 1;
        } else if (!reached_word)
            reached_word = 1;

        if (reached_word && !printed_word)
            putchar(c);

        if (c == '\n') {
            reached_word = 0;
            printed_word = 0;
            putchar(c);
        }
    }

    return 0;
}

#endif 

int main(int argc, char **argv)
{
    int pid;
    int fd[2];
    int filter_res;

    if (argc < 2) {
        fprintf(stderr, "Provide a program as cli arg to launch\n");
        return 1;
    }

    pipe(fd);
    
    pid = fork();
    if (pid == 0) {
        dup2(fd[1], STDOUT_FILENO);
        close(fd[0]);
        close(fd[1]);

        execvp(argv[1], argv+1);
        perror(argv[1]);
        _exit(1);
    }

    close(fd[1]);
    filter_res = filter_pipe(fd[0]);    
    close(fd[0]);
    wait(NULL);
    return filter_res;
}
