/* 5_3/2_53_remake.c */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>

int main(int argc, char **argv)
{
    int in_fd, spc_fd, cnt_fd;

    if (argc < 4) {
        fprintf(stderr, "Specify input file name, file name for lines and "
                        "File name for lenghts\n");
        return 1;
    }

    in_fd = open(argv[1], O_RDONLY);
    if (in_fd == -1) {
        perror(argv[1]);
        return 2;
    }

    spc_fd = open(argv[2], O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (spc_fd == -1) {
        perror(argv[2]);
        close(in_fd);
        return 3;
    }

    cnt_fd = open(argv[2], O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (cnt_fd == -1) {
        perror(argv[3]);
        close(in_fd);
        close(spc_fd);
        return 4;
    }



    close(in_fd);
    close(spc_fd);
    close(cnt_fd);
    return 0;
}
