int sys_write(int fd, const void *buf, int size);

int main(int argc, char **argv)
{
    sys_write(1, "Hello, world\n", 13);
    return 0;
}
