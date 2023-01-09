/* nostdlib/greet2.c (syscall version) */
#include <unistd.h>

static const char dunno[] = "I do not know how to greet you\n";
static const char hello[] = "Hello, dear ";

static int string_length(const char *s)
{
    const char *p;
    for (p = s; *p; p++)
        {}
    return p - s;
}

int main (int argc, char **argv)
{
    if (argc < 2) {
        write(1, dunno, sizeof(dunno)-1);
        return 1;
    }

    write(1, hello, sizeof(hello)-1);
    write(1, argv[1], string_length(argv[1]));
    write(1, "\n", 1);
    
    return 0;
}
