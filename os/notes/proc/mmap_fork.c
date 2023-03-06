/* proc/mmap_fork.c */
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>

int main()
{
    int pgs, pid;
    int *ptr;
    int size = 4096;
    pgs = getpagesize();
    size = ((size-1) / pgs + 1) * pgs;
    ptr = mmap(NULL, size, PROT_READ|PROT_WRITE,
               MAP_SHARED|MAP_ANONYMOUS, 0, 0); /* init mem for shared use */
    if (ptr == MAP_FAILED) {
        fprintf(stderr, "Mmap failed\n");
        return 1;
    }
    pid = fork();
    if (pid == 0) {
        (*ptr)++;
        printf("Child: %d\n", *ptr);
    } else {
        *ptr += 2;
        printf("Parent: %d\n", *ptr);
    }

    return 0;
}
