/* file_io/add_lock_file.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// Jist -- you create an empty lock file with the same name, and if it already
// exists, you wait

// Plus -- no OS interaction, thus will work with net disks
// Minus -- the other programs have to know of the convention, and if the
// program terminates unexpectedly, the lock file will block all other prog-s

int main()
{
    // File locking
    for (;;) {
        int lockd = open("foobar.dat.lock", O_WRONLY|O_CREAT|O_EXCL, 0666);
        if (lockd != -1) {
            close(lockd);
            break; // File created, proceed to do things
        }
        if (errno != EEXIST) {
            perror("foobar.dat.lock");
            return -1;
        }
        
        sleep(1); // So as not to harass the processor
    }

    // File mutation
    int fd = open("foobar.dat", O_WRONLY|O_CREAT|O_APPEND, 0666);
    if (fd == -1) {
        perror("foobar.dat");
        unlink("foobar.dat.lock");
        return -1;
    }

    for (int i = 0; i < 10; i++) {
        char buf[100];
        int pr_res = snprintf(buf, sizeof(buf), 
                              "Here was process %d\n", getpid());
        write(fd, buf, pr_res);

        sleep(1);
    }

    // File unlock
    unlink("foobar.dat.lock");
    return 0;
}
