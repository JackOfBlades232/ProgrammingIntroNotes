/* 5_20.c */
#include <signal.h>
#include <unistd.h>
#include <errno.h>

volatile static sig_atomic_t recieved_int = 0;

const char message1[] = "Press Ctrl-C to quit\n";
const char message2[] = "Good bye\n";

void handler(int s)
{
    int save_errno = errno;
    signal(SIGINT, handler);
    recieved_int = 1;
    write(1, message2, sizeof(message2)-1);
    errno = save_errno;
}

int main()
{
    signal(SIGINT, handler);
    write(1, message1, sizeof(message1)-1);
    while (!recieved_int)
        pause();
    return 0;
}
