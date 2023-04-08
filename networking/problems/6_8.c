/* 6_8.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

enum {
    port = 7654
};

int sock_fd = -1;

void int_handler(int s) {
    if (sock_fd != -1)
        close(sock_fd);
    printf("Connection closed\n");
    exit(EXIT_SUCCESS);
}

int main() {
    signal(SIGINT, int_handler);
    signal(SIGTERM, int_handler);
    
    sock_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (sock_fd == -1) {
        fprintf(stderr, "Failed to open socket\n");
        return EXIT_FAILURE;
    }

    int opt = 1;
    setsockopt(sock_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);

    if (bind(sock_fd, (struct sockaddr *) &addr, sizeof(addr)) == -1) {
        fprintf(stderr, "Failed to bind socket\n");
        close(sock_fd);
        return EXIT_FAILURE;
    }

    if (listen(sock_fd, 5) == -1) {
        fprintf(stderr, "Failed to listen socket\n");
        close(sock_fd);
        return EXIT_FAILURE;
    }

    struct sockaddr_in client_addr;
    socklen_t client_len;
    int connection_sock;
    char reply_buf[128];
    while ((connection_sock = accept(sock_fd,
                    (struct sockaddr *) &client_addr, &client_len)) != -1) {
        time_t t = time(NULL);
        size_t reply_len = sprintf(
            reply_buf, "%sip-address: %s, port: %hu\n", ctime(&t),
            inet_ntoa(client_addr.sin_addr), client_addr.sin_port
        );
        int res = send(connection_sock, reply_buf, reply_len, 0);

        close(connection_sock);
        if (res != reply_len)
            break;
    }

    fprintf(stderr, "Failed to serve client\n");
    close(sock_fd);
    return EXIT_FAILURE;
}
