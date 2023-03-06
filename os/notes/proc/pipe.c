/* proc/pipe.c */
#include <stdlib.h>
#include <unistd.h>

/* another way (for non-related procs) -- FIFO files (spec file type),
 * can be created with mkfifo, can live with nobody attached, to be opened
 * with O_RDONLY or O_WRONLY. Though it looks like a file, it is a pipe in
 * the kernel all the same, not a on-disk file. Both ends should be in work
 * if not, the fist one to put/read something will be blocked */

int main()
{
    /* a pipe is a io stream for proc communication
     * this is an example of how to procs can be connected
     * via a pipe (they have to be spawned by fork) */
    int fd[2];
    int p1, p2;
    pipe(fd); /* creates a pipe with fd[0] == i and fd[1] == o */

    p1 = fork();
    if (p1 == 0) { /* child 1 */
        close(fd[0]); /* c1 will be writing */
        /* write something, will get blocked if pipe buf is full and no one is
         * reading. If one tries to write to a pipe with no one reading, the 
         * proc will get a SIGPIPE, which kills the proc by default. */
        exit(0);
    }

    p2 = fork();
    if (p2 == 0) { /* child 2 */
        close(fd[1]); /* c2 will be reading */
        /* read something, is blocked util something is written to pipe, or
         * until all write descriptors to the pipe are closed (in this case,
         * read will take all that is left in buf, and then will be returning
         * 0 (eof) */
        exit(0);
    }

    /* parent */
    close(fd[0]); /* the parent does not need the descriptors */
    close(fd[1]);
    /* ... */
    return 0;
}
