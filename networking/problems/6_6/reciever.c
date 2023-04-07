/* 6_6/reciever.c */
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>

//#define INET

#ifdef INET
    #include <netinet/in.h>
    #include <arpa/inet.h>
    #define AF AF_INET
#else
    #include <sys/un.h>
    #define AF AF_UNIX
    static const char sock_name[] = "/tmp/6_6_rec.sock";
#endif

int sock_fd = -1;

void int_handler(int s) {
    if (sock_fd != -1)
        close(sock_fd);
#ifndef INET
    unlink(sock_name);
#endif 
    printf("Connection closed\n");
    exit(0);
}

int main(int argc, char **argv) {
#ifdef INET
    if (argc < 2) {
        fprintf(stderr, "Specify args: port\n");
        return -1;
    }
#endif

    signal(SIGINT, int_handler);
    signal(SIGTERM, int_handler);

    sock_fd = socket(AF, SOCK_DGRAM, 0);
    if (sock_fd == -1) {
        fprintf(stderr, "Failed to open socket\n");
        return -2;
    }

#ifdef INET
    struct sockaddr_in addr, from;
    addr.sin_family = AF;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);

    int ok = sscanf(argv[1], "%hd", &addr.sin_port) == 1;
    if (!ok) {
        fprintf(stderr, "Invalid port arg\n");
        return -1;
    }
    addr.sin_port = htons(addr.sin_port);
#else
    struct sockaddr_un addr, from;
    addr.sun_family = AF;
    strncpy(addr.sun_path, sock_name, sizeof(addr.sun_path));
#endif 

    int res = bind(sock_fd, (struct sockaddr *) &addr, sizeof(addr));
    if (res == -1) {
        close(sock_fd);
        fprintf(stderr, "Failed to bind to socket\n");
        return -3;
    }

    socklen_t from_len;
    char buf[128];
    int bytes_rec;

    /* terminates on recieving zero dgramm */
    while ((bytes_rec = recvfrom(sock_fd, buf, sizeof(buf), 0, (struct sockaddr *) &from, &from_len)) > 0) {
#ifdef INET
        printf("IP-Adr: %s, port: %hd\n", inet_ntoa(from.sin_addr), from.sin_port);
#else
        printf("Local socket path: %s\n", from.sun_path);
#endif
        for (size_t i = 0; i < bytes_rec; i++) {
            if (buf[i] != '\n' && buf[i] != '\t' && (buf[i] < 32 || buf[i] > 126))
                putchar('?');
            else
                putchar(buf[i]);
        }
        putchar('\n');
    }

#ifndef INET
    unlink(sock_name);
#endif
    close(sock_fd);
    return bytes_rec == 0 ? 0 : -4;
}
