/* sock.c */
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>

int main() {
    int s_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (s_fd == -1) {
        fprintf(stderr, "Failed to open socket\n");
        return 1;
    }

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(7654); /* host endian short -> net (big) endian */
    /* ports 1..1023 are privileged, only for roor procs */
    addr.sin_addr.s_addr = htonl(INADDR_ANY); /* can just init ip to 0 */

    /* for AF_UNIX - sockaddr_un, for talking inside the machine through
     * spec "socket" filex */

    int res = bind(s_fd, (struct sockaddr *) &addr, sizeof(addr));
    if (res == -1) {
        fprintf(stderr, "Failed to bind\n");
        return 2;
    }

    close(s_fd);
    return 0;
}
