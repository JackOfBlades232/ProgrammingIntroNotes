/* 5_3/create_intfile.c */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>

int main(int argc, char **argv)
{
    int fd;
    int num;

    if (argc < 2) {
        fprintf(stderr, "Provide out file name\n");
        return 1;
    }

    fd = open(argv[1], O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (fd == -1) {
        fprintf(stderr, "Could not open %s\n", argv[1]);
        return 2;
    }

    while (scanf("%d", &num) == 1)
        write(fd, &num, sizeof(num));

    close(fd);
    return 0;
}
