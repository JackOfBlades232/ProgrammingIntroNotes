/* 5_9.c */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

enum {
    time_bufsize = 80
};

void print_file_type(struct stat *st)
{
    printf("type: ");
    if (S_ISREG(st->st_mode))
        printf("regular\n");
    else if (S_ISDIR(st->st_mode))
        printf("directory\n");
    else if (S_ISCHR(st->st_mode))
        printf("character device\n");
    else if (S_ISBLK(st->st_mode))
        printf("block device\n");
    else if (S_ISFIFO(st->st_mode))
        printf("FIFO (named pipe)\n");
    else if (S_ISLNK(st->st_mode))
        printf("symbolic link\n");
    else if (S_ISSOCK(st->st_mode))
        printf("socket\n");
}

void print_access_mode(struct stat *st)
{
    int rights = st->st_mode & 0777;
    int s_bits = (st->st_mode & 07000) >> 9;
    char rights_s[10] = "---------";
    int i, r_mask;

    for (i = 0, r_mask = 1; i < 9; i++, r_mask <<= 1) {
        int is_on = rights & r_mask;
        if (!is_on)
            continue;

        switch (i % 3) {
            case 0:
                rights_s[8-i] = 'x';
                break;
            case 1:
                rights_s[8-i] = 'w';
                break;
            case 2:
                rights_s[8-i] = 'r';
                break;
        }
    }

    printf("acces perms: %s\n", rights_s);
    printf("set-uid %s, set-gid %s, sticky bit %s\n",
            s_bits & 4 ? "on" : "off",
            s_bits & 2 ? "on" : "off",
            s_bits & 1 ? "on" : "off");
}

void print_common_info(struct stat *st)
{
    printf("Storage device id: %ld\n", st->st_dev);
    printf("INode num: %ld\n", st->st_ino);
    printf("Hard link cnt: %ld\n", st->st_nlink);
    printf("UID: %d, GID: %d\n", st->st_uid, st->st_gid);
    if (S_ISCHR(st->st_mode) || S_ISBLK(st->st_mode))
        printf("Device id (for device files): %ld\n", st->st_rdev);
    printf("Byte size: %ld\n", st->st_size);
    printf("FS block size: %ld\n", st->st_blksize);
    printf("Num blocks: %ld\n", st->st_blocks);
}

void print_time(struct timespec time, const char *prefix)
{ 
    char buf[time_bufsize];
    int chars_put;
    time_t sec = time.tv_sec;    
    struct tm *info = localtime(&sec);

    chars_put = strftime(buf, time_bufsize-1, "%c", info);
    buf[chars_put] = '\0';
    printf("%s%s\n", prefix, buf);
}

void print_time_info(struct stat *st)
{
    print_time(st->st_atim, "Last access time: ");
    print_time(st->st_mtim, "Last modification time: ");
    print_time(st->st_ctim, "Last property modification time: ");
}

void print_file_descr(struct stat *st)
{
    print_file_type(st);
    print_access_mode(st);
    print_common_info(st);
    print_time_info(st);
}

int main(int argc, char **argv)
{
    struct stat buf;
    int res;

    if (argc < 2) {
        fprintf(stderr, "Provide file path\n");
        return 1;
    }

    res = lstat(argv[1], &buf);
    if (res == -1) {
        fprintf(stderr, "Could not get stat\n");
        return 2;
    }

    print_file_descr(&buf);

    if (S_ISLNK(buf.st_mode)) {
        res = stat(argv[1], &buf);
        if (res == -1)
            printf("Dangling\n");
        else {
            printf("\nBase file info:\n");
            print_file_descr(&buf);
        }
    }

    return 0;
}
