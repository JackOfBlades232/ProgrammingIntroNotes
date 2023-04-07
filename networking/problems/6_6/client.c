/* 6_6/client.c */
#include <stdio.h>
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

    return 0;
}
