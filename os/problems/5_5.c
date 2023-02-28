/* 5_5.c */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

enum {
    bufsize = 4096
};

int min_int(int a, int b)
{
    return a < b ? a : b;
}

int main(int argc, char **argv)
{
    int fd;

    int base, len;
    char val;
    int scan_res;

    char buf[bufsize];
    char *bufp;

    if (argc < 5) {
        fprintf(stderr, "Specify file name, pos, length and byte val\n");
        return 1;
    }

    fd = open(argv[1], O_WRONLY);
    if (fd == -1) {
        fprintf(stderr, "Couldn't open %s\n", argv[1]);
        return 2;
    }

    scan_res = sscanf(argv[2], "%d", &base) == 1 &&
               sscanf(argv[3], "%d", &len) == 1 &&
               sscanf(argv[4], "%hhd", &val) == 1;
    if (!scan_res) {
        fprintf(stderr, "Invalid arg format: pos, len and byte val must be "
                        "decimal integers\n");
        return 3;
    }

    for (bufp = buf; bufp-buf < min_int(len, bufsize); bufp++)
        *bufp = val;

    lseek(fd, base, SEEK_SET);
    while (len > 0) {
        write(fd, buf, min_int(len, bufsize));
        len -= bufsize;
    }

    close(fd);
    return 0;
}
