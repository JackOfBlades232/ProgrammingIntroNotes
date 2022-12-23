/* 4_23/2_46_remake.c */
#include <stdio.h>

int main(int argc, char **argv)
{
    FILE *fp;
    char text[] = "Humpty Dumpty sat on a wall,\n"
                  "Humpty Dumpty had a great fall.\n"
                  "All the king's horses and all the king's men\n"
                  "Couldn't put Humpty together again.\n";
  
    if (argc < 2) {
        fprintf(stderr, "Provide destination file name!\n");
        return 1;
    }

    fp = fopen(argv[1], "w");
    if (!fp) {
        perror(argv[1]);
        return 2;
    }

    fprintf(fp, "%s", text);

    fclose(fp);
    return 0;
}
