/* 5_6.c */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

enum {
    bufsize = 4096
};

int max_int(int a, int b)
{
    return a > b ? a : b;
}

void xor_encode_buf(char buf[bufsize], int len, int key)
{
    int i;
    int covered_len = max_int(
            ((len-1) / sizeof(key) + 1) * sizeof(key), bufsize);

    for (i = 0; i < covered_len; i += sizeof(key))
        *((int *) (buf+i)) ^= key;
}

int main(int argc, char **argv)
{
    int fd;
    char buf[bufsize];
    int read_res;

    int key;
    int scan_res;

    if (argc < 3) {
        fprintf(stderr, "Provide file name and key for encoding\n");
        return 1;
    }

    fd = open(argv[1], O_RDWR);
    if (fd == -1) {
        fprintf(stderr, "Could not open file %s\n", argv[1]);
        return 2;
    }

    scan_res = sscanf(argv[2], "%d", &key);
    if (scan_res != 1) {
        fprintf(stderr, "Second cli arg must be integer\n");
        return 3;
    }

    while ((read_res = read(fd, buf, bufsize)) != 0) {
        if (read_res == -1) {
            close(fd);
            return 4;
        }

        xor_encode_buf(buf, read_res, key);

        lseek(fd, -read_res, SEEK_CUR);
        write(fd, buf, read_res);
    }

    close(fd);
    return 0;
}
