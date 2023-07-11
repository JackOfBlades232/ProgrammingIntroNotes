/* file_io/flock.c */
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/fcntl.h>

// A bsd-style file lock, which mirrors the readers-writers solution, but,
// sadly, does not include a barrier, thus it is prone to starvation,
// as will be demonstrated. And, it is os-dependent, thus does not word
// on net fs-s. Plus -- the lock is gone when the program is shut down

// This can be fixed in a similar fashion: just add a barrier file and
// use it as a mutex by flock with LOCK_EX

// One can also change the lock on a given fd, it can be blocking
// (LOCK_SH -> LOCK_EX), and it is not atomic

// flock acts on the kernel file_io object, not on the descriptor,
// so if a proc and a child share a descriptor, their flocks will work
// for both of them. This can also happen in a lib func, and might block
// the program. Also be careful with exec* syscalls

#ifdef SHARED
#define LOCK_MODE LOCK_SH // Reader-style
#define LOCK_NAME "shared"
#else
#define LOCK_MODE LOCK_EX // One at a time style
#define LOCK_NAME "exclusive"
#endif

#define LOCKED_SLEEP 1900000
#define UNLOCKED_SLEEP 100000

int main(int argc, char **argv)
{
    int fd, i;

    if (argc < 2) {
        fprintf(stderr, "No argument specified\n");
        return 1;
    }

    fd = open(argv[1], O_RDWR|O_CREAT, 0666);
    if (fd == -1) {
        perror(argv[1]);
        return 2;
    }

    for (i = 0; ; i++) {
        int res;
        
        printf("%d Getting %s lock... ", i, LOCK_NAME);
        fflush(stdout);

        res = flock(fd, LOCK_MODE);
        printf("got it\nNow sleeping\n");
        usleep(LOCKED_SLEEP);

        printf("%d Releasing %s lock... ", i, LOCK_NAME);
        fflush(stdout);

        res = flock(fd, LOCK_UN);
        printf("done\nNow sleeping\n");
        usleep(UNLOCKED_SLEEP);
    }

    return 0;
}
