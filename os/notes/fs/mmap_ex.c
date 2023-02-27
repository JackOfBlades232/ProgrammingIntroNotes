/* fs/mmap_ex.c */
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int main()
{
    int fd, pgs;
    char *p;
    int size = 4096;

    pgs = getpagesize(); /* pagesize for mmap -- map file to virt mem */
    size = ((size-1) / pgs + 1) * pgs;
    /* min int, > size and divisible by page size */

    fd = open("file.dat", O_RDWR);
    if (fd == -1) {
        /* process error */
        return 1;
    }

    p = mmap(NULL, size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    /* different protection and mode for mem. MAP_ANONYMOUS = ignore file and 
     * just get mem (might be used for malloc) */

    if (p == MAP_FAILED) {
        /* process error */
    }

    munmap(p, size); /* revert map. If not stated otherwise, file content will
                        be modified */

    return 0;
}
