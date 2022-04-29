int sys_write(int fd, const void *buf, int size);

static int string_length(const char *s)
{
    int i;
    for(i = 0; s[i]; i++)
        ;
    return i;
}

int main(int argc, char **argv)
{
    int i;
    for(i = 0; i < argc; i++) {
        sys_write(1, argv[i], string_length(argv[i]));
        sys_write(1, "\n", 1);
    }
    return 0;
}
