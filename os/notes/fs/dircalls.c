/* fs/dircalls.c */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>

int main()
{
    struct stat buf;

    link("./sample.txt", "./hardlink_sample");
    /* create hard link, -1 is err */

    unlink("./hardlink_sample");
    /* remove link to disk file, close as can get to delete (real delete only
     * when inode hardlink counter = 0. Does not work for dirs */

    link("./sample.txt", "./hardlink_sample1");
    
    rename("hardlink_sample1", "sample1.txt");
    /* obvious one, can also move, which in fs abstraction is the same
     * Works only inside one phys disk */

    mkdir("sample_dir", 0777); 
    mkdir("sample_dir1", 0777); 
    rmdir("sample_dir1"); 
    /* dir syscalls 
     * Best to use 0777 cause of umask and the meaning of ex bit */

    symlink("hardlink_sample1", "symlink_sample");
    symlink("hardlink_sample1", "symlink_sample1");
    /* creates symlink (separate inode+file with a link to the fs name).
     * oldpath can be any non-empty str, if is giberrish the link will just
     * lead nowhere */

    unlink("symlink_sample");

    chmod("sample.txt", 0600);
    /* reg chmod, only for owner and su, is not affected by umask */

    chown("sample.txt", 1000, -1);
    /* change owner with uid and gid. For symlinks affects the base file.
     * lchown -- same but symlink is treated separately. 
     * -1 in params means leave the same */

    stat("sample.txt", &buf);
    /* read info on file to special struct
     * lstat -- same as lchown for symlinks
     * fstat -- from fd instead of file name */
    printf(
            "Device id (file storage): %ld\n"
            "Inode number: %ld\n"
            "Mode and rights: %d\n"
            "Hardlink cnt: %ld\n"
            "UID: %d\n"
            "GID: %d\n"
            "Device id (for files that are devices): %ld\n"
            "Byte size: %ld\n"
            "IO block size: %ld\n"
            "512-byte block cnt: %ld\n",

            buf.st_dev, buf.st_ino, buf.st_mode, buf.st_nlink, buf.st_uid,
            buf.st_gid, buf.st_rdev, buf.st_size, buf.st_blksize, buf.st_blocks
            );

    truncate("sample.txt", 32);
    /* truncate all in file from pos
     * ftruncate -- for fd */

    /* for devices (=/dev/.. files): ioctl syscall for passing commands */

    return 0;
}
