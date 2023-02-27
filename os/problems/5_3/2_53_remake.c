/* 5_3/2_53_remake.c */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>

int main(int argc, char **argv)
{
    FILE *in_f, *spc_f;
    int cnt_fd;

    int c;
    int cur_line_ln;
    int write_to_spc;

    if (argc < 4) {
        fprintf(stderr, "Specify input file name, file name for lines and "
                        "File name for lenghts\n");
        return 1;
    }

    in_f = fopen(argv[1], "r");
    if (!in_f) {
        perror(argv[1]);
        return 2;
    }

    spc_f = fopen(argv[2], "w");
    if (!spc_f) {
        perror(argv[2]);
        fclose(in_f);
        return 3;
    }

    cnt_fd = open(argv[3], O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (cnt_fd == -1) {
        perror(argv[3]);
        fclose(in_f);
        fclose(spc_f);
        return 4;
    }

    cur_line_ln = 0;
    write_to_spc = 0;
    while ((c = getc(in_f)) != EOF) {
        if (c == '\n' || c == '\r') {
            write(cnt_fd, &cur_line_ln, sizeof(cur_line_ln));
            cur_line_ln = 0;

            if (write_to_spc) {
                putc(c, spc_f);
                write_to_spc = 0;
            }

            continue;
        }

        if (cur_line_ln == 0 && c == ' ')
            write_to_spc = 1;

        if (write_to_spc)
            putc(c, spc_f);

        cur_line_ln++;
    }

    fclose(in_f);
    fclose(spc_f);
    close(cnt_fd);
    return 0;
}
