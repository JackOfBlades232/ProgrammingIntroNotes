/* sock.c */
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

int main()
{
    int s_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (s_fd == -1) {
        fprintf(stderr, "Failed to open socket\n");
        return 1;
    }

    close(s_fd);
    return 0;
}
