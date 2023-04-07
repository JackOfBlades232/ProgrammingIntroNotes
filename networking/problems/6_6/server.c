/* 6_6/server.c */
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Specify args: port\n");
        return -1;
    }

    // The thing

    return 0;
}
