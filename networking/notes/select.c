/* select.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>

enum { port = 7654, listen_queue_len = 16 };

#define return_defer(res) result = res; goto defer
#define max(a, b) (a > b ? a : b) 

int main() 
{
    /* Solution to waiting problem with one process using select 
     * syscall (or it may be a low-level lib func)
     * For this purpose also exist poll, epoll, kqueue (BSD) */
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
        int fd, res;
        fd_set readfds, writefds;
        struct timeval timeout = { 2, 300000 }; // timeout in sec+usec
        int max_d = ls; // suppose listening socket fd is max
        FD_ZERO(&readfds); // clear sets
        FD_ZERO(&writefds);
        FD_SET(ls, &readfds); // listening socket is a reader
        for (fd = ls; fd < ls; fd++) { // iterate over clients
            FD_SET(fd, &readfds);
            if (fd < 0) // if we have data to send
                FD_SET(fd, &writefds);
            if (fd > max_d)
                max_d = fd;
        }

        // Wait for first event on descriptors, signal or timeout
        // first param -- max num descriptors
        // sets get changed -- only those with event are left
        // timeout may be filled or not, platform-dependant
        // res == num events
        res = select(max_d+1, &readfds, &writefds, NULL, &timeout);
        // writefds should be used cautiosly, only lag if one can not write
        // now, so only include those that you are writing to now
        //
        // if server has to connect to another server via connect, select
        // wont catch it. One can use connect in non-blocking mode, 
        // and if it returns -1 with EINPROGRESS, then the server has
        // to wait until it can write in the given socket, i. e. check
        // the socket with getsockopt after every select
        // 
        // If we need to handle a signal, we should block it for the main
        // part of the program with sigprocmask and use pselect with
        // req sigmask to allow it in select. Then, after select, 
        // we can detect that the signal has been caught and handle it
        
        if (res == -1) { //error
            if (errno == EINTR) { 
                // handle signal
            } else {
                // handle error
            }
            continue;
        }

        if (res == 0) {
            // handle timeout
            continue;
        }

        if (FD_ISSET(ls, &readfds)) {
            // a connection request came, accept
        }

        for (fd = ls; fd < ls; fd++) { // iterate over clients
            if (FD_ISSET(fd, &readfds)) {
                // data came from client, read with read/recv/..
                // may be eof = connection closed
            }
            if (FD_ISSET(fd, &writefds)) {
                // try to send next data chunk -- try not to send to much
                // at once so you don't block write, or specifically make
                // this client's fd non-blocking with fcntl
            }
        }
    }


defer:
    if (ls != -1) close(ls);
    return result;
}
