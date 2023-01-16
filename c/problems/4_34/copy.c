/* 4_34/copy.c */
#include "calls.h"

enum { openwr_flags = 0x241 };
enum { bufsize = 4096 };

static char help_msg[] = "Usage: copy <src> <dest>\n";
static char src_err_msg[] = "Couldn't open source file for reading\n";
static char dest_err_msg[] = "Couldn't open destination file for writing\n";

static char buf[bufsize];

int main (int argc, char **argv)
{
    int src_fd, dest_fd;

    if (argc < 3) {
        sys_write(2, help_msg, sizeof(help_msg)-1);
        return 1;
    }

    src_fd = sys_open(argv[1], 0); /* mode=readonly */
    if (src_fd == -1) {
        sys_write(2, src_err_msg, sizeof(src_err_msg)-1);
        return 2;
    }

    dest_fd = sys_open_chmod(argv[2], openwr_flags, 0666); /* rights=rwrwrw */
    if (dest_fd == -1) {
        sys_write(2, dest_err_msg, sizeof(dest_err_msg)-1);
        sys_close(src_fd);
        return 3;
    }

    for (;;) {
        int bytes_read = sys_read(src_fd, buf, bufsize);

        if (bytes_read <= 0)
            break;

        sys_write(dest_fd, buf, bytes_read);
    }

    sys_close(src_fd);
    sys_close(dest_fd);
    return 0;
}
