/* 5_7/item_list.c */
#include "item_list.h"
#include "item_record.h"
#include "strings.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

static int fadd_record(int fd, const char *item_name)
{
    struct item_record rec;
    int read_res = sizeof(rec);
    int res;
    
    lseek(fd, 0, SEEK_SET);
    while ((read_res = read_record(fd, 0, SEEK_CUR, &rec)) == sizeof(rec)) {
        if (strings_are_equal(item_name, rec.name)) {
            rec.cnt++;
            res = write_record(fd, -1, SEEK_CUR, &rec) == sizeof(rec);
            close(fd);
            return res;
        }
    }

    if (read_res != 0) {
        close(fd);
        return 0;
    }

    string_copy(item_name, rec.name, sizeof(rec.name));
    rec.cnt = 1;

    return write_record(fd, 0, SEEK_END, &rec) == sizeof(rec);
}

int add_record(const char *filepath, const char *item_name)
{
    int fd;
    int res;
    
    fd = open(filepath, O_RDWR | O_CREAT, 0666);
    if (fd == -1)
        return 0;

    res = fadd_record(fd, item_name);
    close(fd);
    return res;
}

static int fquery_record(int fd, const char *item_name)
{
    struct item_record rec;
    int read_res = sizeof(rec);

    lseek(fd, 0, SEEK_SET);
    while ((read_res = read_record(fd, 0, SEEK_CUR, &rec)) == sizeof(rec)) {
        if (strings_are_equal(item_name, rec.name))
            return rec.cnt;
    }

    return read_res == 0 ? 0 : -1;
}

int query_record(const char *filepath, const char *item_name)
{
    int fd;
    int res;
    
    fd = open(filepath, O_RDONLY);
    if (fd == -1)
        return -1;

    res = fquery_record(fd, item_name);
    close(fd);
    return res;
}

int list_records(const char *filepath, FILE *out_f)
{
    int fd;
    struct item_record rec;
    int read_res = sizeof(rec);
    
    fd = open(filepath, O_RDONLY);
    if (fd == -1)
        return 0;

    while ((read_res = read_record(fd, 0, SEEK_CUR, &rec)) == sizeof(rec))
        fprintf(out_f, "%s: %d\n", rec.name, rec.cnt);

    close(fd);
    return read_res == 0;
}

int join_lists(const char *in_filepath1, const char *in_filepath2, 
        const char *out_filepath)
{
    int in_fd1 = -1, in_fd2 = -1, out_fd = -1;
    struct item_record rec;
    int read_res = sizeof(rec);
    
    in_fd1 = open(in_filepath1, O_RDONLY);
    if (in_fd1 == -1)
        goto join_error;

    in_fd2 = open(in_filepath2, O_RDONLY);
    if (in_fd2 == -1)
        goto join_error;

    out_fd = open(out_filepath, O_WRONLY | O_CREAT, 0666);
    if (out_fd == -1)
        goto join_error;

    while ((read_res = read_record(in_fd1, 0, SEEK_CUR, &rec)) == sizeof(rec)) {
        int q_res = fquery_record(in_fd2, rec.name);
        if (q_res == -1)
            goto join_error;

        rec.cnt += q_res;
        write_record(out_fd, 0, SEEK_CUR, &rec);
    }

    lseek(in_fd2, 0, SEEK_SET);
    while ((read_res = read_record(in_fd2, 0, SEEK_CUR, &rec)) == sizeof(rec)) {
        int q_res = fquery_record(in_fd1, rec.name);
        if (q_res == -1)
            goto join_error;

        if (q_res == 0)
            write_record(out_fd, 0, SEEK_CUR, &rec);
    }

    close(in_fd1);
    close(in_fd2);
    close(out_fd);
    return 1;

join_error:
    if (in_fd1 != -1)
        close(in_fd1);
    if (in_fd2 != -1)
        close(in_fd2);
    if (out_fd != -1)
        close(out_fd);
    return 0;
}
