/* 5_4.c */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>

enum {
    bufsize = 4096
};

int char_is_eol(char c)
{
    return c == '\n' || c == '\r';
}

void parse_buf(char *buf, int len, int *line_cnt)
{
    char *bufp;
    for (bufp = buf; bufp-buf < len; bufp++) {
        if (char_is_eol(*bufp))
            (*line_cnt)++;
    }
}

int main(int argc, char **argv)
{
    int fd;
    char buf[bufsize];
    int read_res, line_cnt;

    if (argc < 2) {
        fprintf(stderr, "Specify file name\n");
        return 1;
    }

    fd = open(argv[1], O_RDONLY);
    if (fd == -1) {
        fprintf(stderr, "Could not open %s\n", argv[1]);
        return 2;
    }

    line_cnt = 0;
    while ((read_res = read(fd, buf, bufsize)) != 0) {
        if (read_res == -1)
            goto read_err;

        parse_buf(buf, read_res, &line_cnt);
    }

    printf("Line count: %d\n", line_cnt);
    close(fd);
    return 0;

read_err:
    close(fd);
    return 3;
}
