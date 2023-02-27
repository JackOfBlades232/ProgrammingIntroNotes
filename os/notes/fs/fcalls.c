/* fs/fcalls.c */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int main()
{
    int fd;
    int um;
    int flags;
    int bytes_read;

    char buf[128];

    um = umask(022); 
    /* sets umask for dwr for this proc and all children
     * (does not affect set uid, set gid or sticky bit)
     * umask is applied as to rights as or with negated umask val.
     * Return val is prev umask val */

    fd = open("sample.txt", O_RDWR | O_APPEND | O_CREAT, 0666);
    /* Some mode params for open:
     * O_RDOLNY or O_WRONLY or O_RDWR (can choose 1, others with |)
     * O_APPEND -- write to end of file
     * O_CREAT -- if does not exist, create
     * O_TRUNC -- if exists, erase contents
     * O_EXCL -- only with O_CREAT, if exists raise error
     * O_NONBLOCK -- read/write syscalls don't block process
     *
     * perms -- value for chmod, only req and used with O_CREAT. 
     * Usually only 0666 and 0600 make sense, since all else can be dealt with
     * with umask
     *
     * Ret: -1 -- error, else -- fd number */

    if (fd < 0) {
        /* communicate error */
        goto deinit;
    }

    bytes_read = read(fd, buf, sizeof(buf));
    if (bytes_read > 0)
        write(fd, buf, bytes_read);
    /* write and read work like in asm, with -1 for err, 0 for eof (read)
     * and > 0 for chars read.
     *
     * To note -- if there is at least something in the kernel buffer,
     * the syscall will take what it has, and it can be < len, to prevent
     * blocking */

    lseek(fd, 7, SEEK_END); /* not all streams support lseek */
    /* change pos in file: second param -- offset, third -- base for offset, 
     * can be start, end or cur pos. Can offset further than end, then the 
     * hole will be filled with 0es. Also has var: lseek64 for ll offset,
     * only valid for 32bit arch since on 64bits l = ll */
    write(fd, "New content", 11); 

    /* Piece of trivia: most io nowadays is async -- system bufferises write
     * res, and returns control straight away. This is why you might want
     * to control it a little */
    fsync(fd);
    /* tells os to write all bufferised output for a descriptor asap. Also
     * rets 0 or -1, and is always blocking. */

    fdatasync(fd);
    /* more useful variant, ignores metadata for inode */

    sync();
    /* as fsync, but for all process descriptors.
     * Note: syncs are requests and block process, they do not raise their
     * priority for the kernel. */

    flags = fcntl(fd, F_GETFL);
    /* fcntl -- universal "perform command on fd" */
    if (!(flags & O_APPEND))
        fcntl(fd, F_SETFL, O_APPEND);

    close(fd);
    /* close - obvious syscall. Should close temp files, since descriptor
     * amount in system is limited. Returns -1 if error, it will still close,
     * but it might be a sign of error when system tried to write bufferised
     * data */

deinit:
    return 0;
}
