/* 4_34/calls.h */
#ifndef CALLS_SENTRY
#define CALLS_SENTRY

int sys_open(const char *path, int mode);
int sys_open_chmod(const char *path, int mode, int rights);
int sys_close(int fd);
int sys_read(int fd, void *buf, int size);
int sys_write(int fd, const void *buf, int size);

#endif
