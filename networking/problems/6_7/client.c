/* 6_7/client.c */
#include <signal.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define MAX_UDP_PACKSIZE 508
#define ALARM_DELAY 15

int sock_fd = -1;

void alarm_handler(int s) {
    if (sock_fd != -1)
        close(sock_fd);
    printf("Timeout\n");
    exit(EXIT_SUCCESS);
}

void int_handler(int s) {
    if (sock_fd != -1)
        close(sock_fd);
    printf("Interrupted\n");
    exit(EXIT_FAILURE);
}

int main(int argc, char **argv) {
    if (argc < 3) {
        fprintf(stderr, "Specify args: reciever ip-address and port\n");
        return EXIT_FAILURE;
    }

    signal(SIGINT, int_handler);
    signal(SIGTERM, int_handler);
    signal(SIGALRM, alarm_handler);

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

    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd == -1) {
        fprintf(stderr, "Failed to open socket\n");
        return EXIT_FAILURE;
    }

    char dgram[MAX_UDP_PACKSIZE];
    int dgram_len;
    printf("Input dgram len: ");
    scan_res = scanf("%u", &dgram_len);
    if (scan_res != 1 || dgram_len > MAX_UDP_PACKSIZE) {
        fprintf(stderr, "Invalid dgram size\n");
        close(sock_fd);
        return EXIT_FAILURE;
    }

    int res = sendto(sock_fd, dgram, dgram_len, 0,
            (struct sockaddr *) &serv_addr, sizeof(serv_addr));
    if (res != dgram_len) {
        fprintf(stderr, "Failed to send full dgram\n");
        close(sock_fd);
        return EXIT_FAILURE;
    }

    alarm(ALARM_DELAY);

    char reply_msg[128];
    socklen_t slen;
    res = recvfrom(sock_fd, reply_msg, sizeof(reply_msg), 0,
            (struct sockaddr *) &serv_addr, &slen);
    if (res == -1) {
        fprintf(stderr, "Failed to recieve reply\n");
        close(sock_fd);
        return EXIT_FAILURE;
    }

    printf("%s", reply_msg);

    close(sock_fd);
    return EXIT_SUCCESS;
}
