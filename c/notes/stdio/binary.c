/* binary.c */
/* prototypes of functions for working with block-binary files, but usually
 * people use low-level syscall wrappers for this */
#include <stdio.h>

int main()
{
    FILE *stream;
    int elem, res;

    stream = fopen("ex.bin", "r+");
    if (!stream) {
        perror("ex.bin");
        return 1;
    }

    while (!feof(stream) && !ferror(stream)) {
        res = fread(&elem, sizeof(elem), 1, stream); /* returns number of read
                                                        elements */
        fseek(stream, -1, SEEK_CUR); /* SEEK_CUR, SEEK_SET, SEEK_END -- offsets
                                        from cur place, start, end, returns
                                        new pos */
        fwrite(&elem, sizeof(elem), 1, stream);
    }

    /* REMEMBER: not all streams support positioning, obviously */

    fclose(stream);
    return 0;
}
