/* 5_33.c */
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <string.h>

int main(int argc, char **argv)
{
    int status = 0;
    int matched;

    struct termios ts1, ts2;

    FILE *f;
    int c;

    if (!isatty(0)) {
        fprintf(stderr, "Not a terminal\n");
        return 1;
    }

    if (argc < 2) {
        fprintf(stderr, "Specify pswd file name\n");
        return 1;
    }

    f = fopen(argv[1], "r");
    if (!f) {
        fprintf(stderr, "Could not open pswd file\n");
        return 1;
    }

    tcgetattr(0, &ts1); 
    memcpy(&ts2, &ts1, sizeof(ts1)); 
    ts1.c_lflag &= ~ECHO; 
    tcsetattr(0, TCSANOW, &ts1); 

    printf("Please blind-type the code: ");

    matched = 0;
    while ((c = getchar()) == fgetc(f)) {
        if (c == EOF) {
            fprintf(stderr, "Unexpected EOF\n");
            status = 1;
            break;
        } else if (c == '\n') {
            matched = 1;
            break;
        }
    }
    
    putchar('\n');
    if (status == 0 && !matched) {
        printf("incorrect password\n");
        status = 1;
    }

    tcsetattr(0, TCSANOW, &ts2); 
    return status;
}
