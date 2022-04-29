#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>


#ifdef SHARED
#define LOCK_MODE LOCK_SH
#define LOCK_NAME "shared"
#else
#define LOCK_MODE LOCK_EX
#define LOCK_NAME "exclusive"
#endif
#define LOCKED_SLEEP  1900000
#define UNLOCKED_SLEEP 100000


int main(int argc, char** argv)
{
    int i;
    if(argc < 2) {
        fprintf(stderr, "No argument specified\n");
        return 1;
    }
    int fd = open(argv[1], O_RDWR|O_CREAT, 0666);
    if(fd == -1) {
        perror(argv[1]);
        return 2;
    }
    for(i = 0;;i++) {
        int res;
        printf("%d Getting %s lock... ", i, LOCK_NAME); fflush(stdout);
        res = flock(fd, LOCK_MODE);
        res = flock(fd, LOCK_MODE);
        printf("got it (%d)\nNow sleeping\n", res);
        usleep(LOCKED_SLEEP);
        printf("%d Releasing %s lock... ", i, LOCK_NAME); fflush(stdout);
        res = flock(fd, LOCK_UN);
        printf("done\nNow sleeping\n");
        usleep(UNLOCKED_SLEEP);
    }
}
