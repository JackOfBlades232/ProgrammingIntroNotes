/* multiproc_server.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

enum { port = 7654, listen_queue_len = 16 };

#define return_defer(res) result = res; goto defer

int main() 
{
    /* Solution to the problem of waiting in TCP servers 
     * using separate processes for each connection */
    int result = 0;

    int ls = -1, ok;
    struct sockaddr_in addr;

    ls = socket(AF_INET, SOCK_STREAM, 0);
    if (ls == -1) {
        fprintf(stderr, "Failed to open socket\n");
        return_defer(-1);
    }

    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    ok = bind(ls, &addr, sizeof(addr));
    if (ok == -1) {
        fprintf(stderr, "Failed to bind socket\n");
        return_defer(-1);
    }

    listen(ls, listen_queue_len);
    for (;;) {
        int cls, pid;
        socklen_t slen = sizeof(addr);
        cls = accept(ls, &addr, &slen); // main proc only accepts
        if (cls == -1) {
            fprintf(stderr, "Error in accept\n");
            return_defer(-1);
        }
        
        pid = fork(); // and each child serves one connection
        if (pid == 0) {
            close(ls);
            /* Serve client */
            close(cls);
            exit(0);
        }

        close(cls);
        do {
            pid = wait4(-1, NULL, WNOHANG, NULL);
        } while (pid > 0); // non-blocking wait cycle for zombies
    }

defer:
    if (ls != -1) close(ls);
    return result;
}
