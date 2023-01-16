/* 4_33/io.c */

int sys_read(int fd, void *buf, int size);
int sys_write(int fd, const void *buf, int size);

static int strlen(const char *s)
{
    const char *p;
    for (p = s; *p; p++)
        {}
    return p - s;
}

int read_input_to_buf(char *buf, int len)
{
    int res;
    res = sys_read(0, buf, len - 1);
    buf[res] = '\0';
    return res;
}

int write_output_from_buf(const char *buf, int len)
{
    return sys_write(1, buf, len);
}

int writec(char c)
{
    return sys_write(1, &c, 1);
}

static int write_str(int fd, const char *s)
{
    int len;
    len = strlen(s);
    return sys_write(fd, s, len);
}

int werr(const char *msg)
{
    return write_str(2, msg);
}
