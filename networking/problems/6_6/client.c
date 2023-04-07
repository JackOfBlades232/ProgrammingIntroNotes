/* 6_6/client.c */
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main(int argc, char **argv) {
    if (argc < 4) {
        fprintf(stderr, 
            "Specify args: reciever ip-address, port, dgram content\n");
        return -1;
    }

    struct sockaddr_in serv_addr;
    unsigned short serv_port;
    serv_addr.sin_family = AF_INET;

    int ok = inet_aton(argv[1], &serv_addr.sin_addr);
    if (!ok) {
        fprintf(stderr, "Provide valid server ip-address\n");
        return EXIT_FAILURE;
    }

    int scan_res = sscanf(argv[2], "%hu", &serv_port);
    if (scan_res != 1) {
        fprintf(stderr, "Provide valid server port\n");
        return EXIT_FAILURE;
    }
    serv_addr.sin_port = htons(serv_port);

    int sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd == -1) {
        fprintf(stderr, "Failed to open socket\n");
        return EXIT_FAILURE;
    }

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(0);
    addr.sin_addr.s_addr = INADDR_ANY;

    int res = bind(sock_fd, (struct sockaddr *) &addr, sizeof(addr));
    if (res == -1) {
        fprintf(stderr, "Failed to bind to socket\n");
        close(sock_fd);
        return EXIT_FAILURE;
    }

    size_t dgram_len = strlen(argv[3]);
    res = sendto(sock_fd, argv[3], dgram_len, 0,
            (struct sockaddr *) &serv_addr, sizeof(serv_addr));
    if (res != dgram_len) {
        fprintf(stderr, "Failed to send full dgram\n");
        close(sock_fd);
        return EXIT_FAILURE;
    }

    close(sock_fd);
    return 0;
}
