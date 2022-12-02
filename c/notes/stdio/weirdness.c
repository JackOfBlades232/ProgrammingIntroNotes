/* weirdness.c */
#include <stdio.h>

int main()
{
    puts("Hello world"); /* unlike fputs, it also newlines */
    /* gets is HIGHLY ILLEGAL, as it does not take array size, and prog might
     * not crash, but overwrite other mem in stack or on heap. A hacker can
     * provide a longer str, and override the return adr (was in base of stack
     * frame), so he can call any func, or he can even place his own machine
     * code there. (put code in stack, and it's adr as return adr) */

    return 0;
}
