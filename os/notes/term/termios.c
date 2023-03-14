/* term/termios.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main()
{
    struct termios *tp; /* struct for all term settings */

    /* some of termios fields (really, tcflag_t and cc_t)
     * int c_iflag; input flags (info from term)
     * int c_oflag; output flags (to term)
     * int c_cflag; mostly used for work with physical line
     * int c_lflag; on the contrary, local options mostly here */

    return 0;
}
