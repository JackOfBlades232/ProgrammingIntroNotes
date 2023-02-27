/* 5_1.c */
#define _LARGEFILE64_SOURCE

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>

int main(int argc, char **argv)
{
    int fd;
    long long byte_len;

    if (argc < 2) {
        fprintf(stderr, "Specify file name\n");
        return 1;
    }

    fd = open(argv[1], O_RDONLY);
    if (fd == -1) {
        perror(argv[1]);
        return 2;
    }

    byte_len = lseek64(fd, 0, SEEK_END);
    printf("Byte len of %s: %lld\n", argv[1], byte_len);
    return 0;
}
