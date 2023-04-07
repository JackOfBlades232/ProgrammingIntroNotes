/* 6_6/sender.c */
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>

// #define INET

#ifdef INET
    #include <netinet/in.h>
    #include <arpa/inet.h>
    #define AF AF_INET
    #define R_ARGC 4
#else
    #include <sys/un.h>
    #define AF AF_UNIX
    #define R_ARGC 2
    static const char sock_name[] = "/tmp/6_6_snd.sock";
    static const char reciever_name[] = "/tmp/6_6_rec.sock";
#endif

int sock_fd = -1;

void int_handler(int s) {
    if (sock_fd != -1)
        close(sock_fd);
#ifndef INET
    unlink(sock_name);
#endif 
    exit(0);
}

int main(int argc, char **argv) {
    if (argc < R_ARGC) {
        fprintf(stderr, "Specify args: reciever ip-address, port, dgram content\n");
        return -1;
    }

    signal(SIGINT, int_handler);
    signal(SIGTERM, int_handler);

    sock_fd = socket(AF, SOCK_DGRAM, 0);
    if (sock_fd == -1) {
        fprintf(stderr, "Failed to open socket\n");
        return -2;
    }

#ifdef INET
    struct sockaddr_in addr, to;
    addr.sin_family = AF;
    addr.sin_port = htons(0);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);

    to.sin_family = AF;
    int ok = sscanf(argv[2], "%hd", &to.sin_port) &&
             inet_aton(argv[1], &addr.sin_addr);
    if (!ok) {
        fprintf(stderr, "Invalid port or ip-address arg\n");
        return -1;
    }

    to.sin_port = htons(to.sin_port);
#else
    struct sockaddr_un addr, to;
    addr.sun_family = AF;
    strncpy(addr.sun_path, sock_name, sizeof(addr.sun_path));

    to.sun_family = AF;
    strncpy(to.sun_path, reciever_name, sizeof(addr.sun_path));
#endif 

    int res = bind(sock_fd, (struct sockaddr *) &addr, sizeof(addr));
    if (res == -1) {
        close(sock_fd);
        fprintf(stderr, "Failed to bind to socket\n");
        return -3;
    }

    size_t msg_len = strlen(argv[R_ARGC-1]);
    res = sendto(
        sock_fd, argv[R_ARGC-1], msg_len, 0,
        (const struct sockaddr *) &to, sizeof(to)
    );

#ifndef INET
    unlink(sock_name);
#endif
    close(sock_fd);

    if (res != msg_len) {
        fprintf(stderr, "Failed to send full dgram\n");
        return -4;
    }

    return 0;
}
