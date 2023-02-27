/* 5_2.c */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int main()
{
    char buf[4096];
    int read_res = 1;

    while ((read_res = read(STDIN_FILENO, buf, sizeof(buf))) > 0)
        write(STDOUT_FILENO, buf, read_res);

    if (read_res == -1) {
        fprintf(stderr, "Error while reading input\n");
        return 1;
    }

    return 0;
}
