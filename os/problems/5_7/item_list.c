/* 5_7/item_list.c */
#include "item_list.h"
#include "strings.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

static void seek_record(int fd, int rec_offset, int seek_base)
{
    if (rec_offset != 0 || seek_base != SEEK_CUR) 
        lseek(fd, rec_offset*sizeof(struct item_record), seek_base);
}

static int write_record(int fd, int rec_offset, int seek_base,
        const struct item_record *item)
{
    seek_record(fd, rec_offset, seek_base);
    return write(fd, item->name, sizeof(item->name)) +
        write(fd, &item->cnt, sizeof(item->cnt));
}

static int read_record(int fd, int rec_offset, int seek_base, 
        struct item_record *out)
{
    seek_record(fd, rec_offset, seek_base);
    return read(fd, out->name, sizeof(out->name)) +
        read(fd, &out->cnt, sizeof(out->cnt));
}

int add_record(const char *filepath, const char *item_name)
{
    int fd;
    struct item_record rec;
    int read_res = sizeof(rec);
    int res;
    
    fd = open(filepath, O_RDWR | O_CREAT, 0666);
    if (fd == -1)
        return 0;

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

    res = write_record(fd, 0, SEEK_END, &rec) == sizeof(rec);
    close(fd);
    return res;
}

int query_record(const char *filepath, const char *item_name)
{
    int fd;
    struct item_record rec;
    int read_res = sizeof(rec);
    
    fd = open(filepath, O_RDONLY);
    if (fd == -1)
        return -1;

    while ((read_res = read_record(fd, 0, SEEK_CUR, &rec)) == sizeof(rec)) {
        if (strings_are_equal(item_name, rec.name))
            return rec.cnt;
    }

    close(fd);
    return read_res == 0 ? 0 : -1;
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
