/* blindpsw.c */
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <string.h>

enum { bufsize = 128 };

int main()
{
    struct termios ts1, ts2;
    char buf[bufsize];

    if (!isatty(0)) {
        fprintf(stderr, "Not a terminal\n");
        return 1;
    }

    tcgetattr(0, &ts1); /* get current termios settings */
    memcpy(&ts2, &ts1, sizeof(ts1)); /* create a copy */
    ts1.c_lflag &= ~ECHO; /* unset echo flag */
    tcsetattr(0, TCSANOW, &ts1); /* deactivate local echo flag */

    printf("Please blind-type the code: ");
    if (!fgets(buf, sizeof(buf), stdin)) {
        fprintf(stderr, "Unexpected EOF\n");
        return 2;
    }

    printf("\nThe code you entered is [%s]\n", buf);
    tcsetattr(0, TCSANOW, &ts2); /* restore settings */
    return 0;
}
