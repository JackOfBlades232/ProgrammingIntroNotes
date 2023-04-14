/* select.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>

enum { port = 7654, listen_queue_len = 16 };

#define return_defer(res) result = res; goto defer

int main() 
{
    int result = 0;
    int ls = -1;

    // Unfinished

defer:
    if (ls != -1) close(ls);
    return result;
}
