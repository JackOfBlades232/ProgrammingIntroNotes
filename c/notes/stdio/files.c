/* files.c */
#include <stdio.h>
#include <errno.h> /* access to global var -- std lib func calls err code */
#include <stdlib.h>
#include <unistd.h>

/* flags: r -- read, r+ -- read/write, start from beginning, w -- write, 
 * create if needed, delete all contents, w+ -- same, but can also read,
 * a -- append to end, a+ -- append to end with reading from start. 
 * rb, wb+, a+b ... -- indicates that the file is binary. On unix it is 
 * ignored, but on Windows -- no */

/* high-level io uses c lib runtime buffers, and when you input -- you fill 
 * buffer, and it is not emptied immediately, so an error in fclose is when
 * it was impossible to flush input buffer. The fd will be still closed */

int main()
{
    FILE *f; /* just a pointer to whatever a file is */
    f = fopen("file.txt", "r"); /* often ends with error and sets errno */
    if (!f) {
        perror("file.txt"); /* checks errno and prints err */
        exit(1);            /*   with context to stderr */
    }
    if (isatty(1)) { /* checks if a live user is beyond an fd (int) */
        fflush(f); /* for input discards buffer, 
                      for output gives it all to OS */
    }
    fclose(f); /* can also return -1 as err */

    return 0;
}
