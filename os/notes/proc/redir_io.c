/* proc/redir_io.c */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int main()
{
    /* let us try to put a file in the place of stdout */
    int save1, fd; 
    fflush(stdout);

    save1 = dup(1); /* save init stdout */
    fd = open("file.dat", O_CREAT|O_WRONLY|O_TRUNC, 0666); /* our file */

    if (fd == -1) {
        /* handle err */
        return 1;
    }

    dup2(fd, 1); /* make stdout a descriptor of the file */
    close(fd); /* close unneeded descr */

    /* do your thing */

    dup2(save1, 1); /* restore stdout */
    close(save1); /* remove copy */

    return 0;
}
