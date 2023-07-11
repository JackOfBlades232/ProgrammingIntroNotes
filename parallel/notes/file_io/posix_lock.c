/* file_io/posix_lock.c */
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/fcntl.h>

// Posix locks can lock parts of the file, and do not affect the 
// kernel io object, but create a relation between a proc and an on-disk file.
// So new opened fds in proc inherit the lock.

// However, posix locks are not inherited by forked processes

// As a side note, newer versions of netfs and kernels allow posix locks on
// net file systems

// Another thing -- closing of any fd with a file relieves it of all
// posix locks with this proc. This is done to unlock everything on
// process termination, but can lead to strange effects if you try
// to dup a descriptor and close one of them, or if a lib func reopens
// your file and closes it back

// Also, there are some strange thing with rights to apply locks))

#ifdef SHARED
#define LOCK_NAME "shared"
#else
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

    // For posix locks the file cannot be opened as O_RDONLY
    fd = open(argv[1], O_RDWR|O_CREAT, 0666);
    if (fd == -1) {
        perror(argv[1]);
        return 2;
    }

    int file_len = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);

    for (i = 0; ; i++) {
        int res;
#ifdef SHARED
        struct flock flk;
#endif
        
        printf("%d Getting %s lock... ", i, LOCK_NAME);
        fflush(stdout);

#ifdef SHARED
        // With fcntl you can do more stuff, like shared locks
        // If ok, l_type is set to UNLCK, otherwise the struct gets filled
        // out with the info of the current lock (if F_SETLK)
        // F_SETLKW gets blocked and waits
        flk.l_type = F_RDLCK; // shared lock
        flk.l_start = 0;
        flk.l_whence = SEEK_SET;
        flk.l_len = file_len;
        res = fcntl(fd, F_SETLKW, &flk);
#else
        // lockf is simpler, but only has exclusive locks
        res = lockf(fd, F_LOCK, file_len);
#endif
        printf("got it\nNow sleeping\n");
        usleep(LOCKED_SLEEP);

        printf("%d Releasing %s lock... ", i, LOCK_NAME);
        fflush(stdout);

#ifdef SHARED
        // One can also unlock segments, and the remainder still is locked
        flk.l_type = F_UNLCK;
        flk.l_start = 0;
        flk.l_whence = SEEK_SET;
        flk.l_len = file_len;
        res = fcntl(fd, F_SETLKW, &flk);
#else
        res = lockf(fd, F_ULOCK, file_len);
#endif
        printf("done\nNow sleeping\n");
        usleep(UNLOCKED_SLEEP);
    }

    return 0;
}
