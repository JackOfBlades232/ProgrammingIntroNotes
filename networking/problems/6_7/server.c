/* 6_7/server.c */
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define MAX_UDP_PACKSIZE 508

int sock_fd = -1;

void int_handler(int s) {
    if (sock_fd != -1)
        close(sock_fd);
    printf("Connection closed\n");
    exit(EXIT_SUCCESS);
}

void daemonize() {
    int pid;

    close(STDIN_FILENO); 
    close(STDOUT_FILENO);
    close(STDERR_FILENO);
    open("/dev/null", O_RDONLY);
    open("/dev/null", O_WRONLY);
    open("/dev/null", O_WRONLY);

    chdir("/");

    pid = fork();
    if (pid > 0)
        exit(0);
    setsid();

    pid = fork();
    if (pid > 0)
        exit(0);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Specify args: port\n");
        return EXIT_FAILURE;
    }

    unsigned short port;
    int scan_res = sscanf(argv[1], "%hu", &port);
    if (scan_res != 1) {
        fprintf(stderr, "Provide valid port\n");
        return EXIT_FAILURE;
    }

    signal(SIGINT, int_handler);
    signal(SIGTERM, int_handler);

    daemonize();

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

    struct sockaddr from;
    socklen_t fromlen;
    char buf[MAX_UDP_PACKSIZE];
    int bytes_recieved;

    int dgram_cnt = 0;
    int total_dgram_size = 0;
    char response_buf[128];

    while ((bytes_recieved = recvfrom(sock_fd, &buf, sizeof(buf),
                    0, &from, &fromlen)) != -1) {
        dgram_cnt++;
        total_dgram_size += bytes_recieved;
        sprintf(response_buf, "Total dgram cnt: %d, total size: %d\n", 
                dgram_cnt, total_dgram_size);

        size_t msg_len = strlen(response_buf);
        res = sendto(sock_fd, response_buf, msg_len, 0, &from, fromlen);
        if (res != msg_len)
            break;
    }

    fprintf(stderr, "Error while waiting for/sending dgram\n");
    close(sock_fd);
    return EXIT_FAILURE;
}
