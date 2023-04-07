/* 6_6/server.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int sock_fd = -1;

void int_handler(int s) {
    if (sock_fd != -1)
        close(sock_fd);
    printf("Connection closed\n");
    exit(EXIT_SUCCESS);
}

int char_is_intelligible(char c) {
    return c == '\n' || c == '\t' || (c >= ' ' && c <= '~');
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Specify args: port\n");
        return EXIT_FAILURE;
    }

    signal(SIGINT, int_handler);
    signal(SIGTERM, int_handler);

    unsigned short port;
    int scan_res = sscanf(argv[1], "%hu", &port);
    if (scan_res != 1) {
        fprintf(stderr, "Provide valid port\n");
        return EXIT_FAILURE;
    }

    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd == -1) {
        fprintf(stderr, "Failed to open socket\n");
        return EXIT_FAILURE;
    }

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = INADDR_ANY;

    int res = bind(sock_fd, (struct sockaddr *) &addr, sizeof(addr));
    if (res == -1) {
        fprintf(stderr, "Failed to bind to socket\n");
        close(sock_fd);
        return EXIT_FAILURE;
    }

    struct sockaddr_in from;
    socklen_t fromlen;
    char buf[64];
    int bytes_recieved;

    while ((bytes_recieved = recvfrom(sock_fd, &buf, sizeof(buf), 0,
                    (struct sockaddr *) &from, &fromlen)) != -1) {
        printf("Client ip-address: %s, port: %hu\n",
                inet_ntoa(from.sin_addr), from.sin_port);

        for (char *bp = buf; bp-buf < bytes_recieved; bp++) {
            if (char_is_intelligible(*bp))
                putchar(*bp);
            else
                putchar('?');
        }

        putchar('\n');
    }

    fprintf(stderr, "Error while waiting for dgram\n");
    close(sock_fd);
    return EXIT_FAILURE;
}
