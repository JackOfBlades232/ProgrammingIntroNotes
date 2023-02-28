/* 5_7/item_record.c */
#include "item_record.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

static void seek_record(int fd, int rec_offset, int seek_base)
{
    if (rec_offset != 0 || seek_base != SEEK_CUR) 
        lseek(fd, rec_offset*sizeof(struct item_record), seek_base);
}

int write_record(int fd, int rec_offset, int seek_base,
        const struct item_record *item)
{
    seek_record(fd, rec_offset, seek_base);
    return write(fd, item->name, sizeof(item->name)) +
        write(fd, &item->cnt, sizeof(item->cnt));
}

int read_record(int fd, int rec_offset, int seek_base, 
        struct item_record *out)
{
    seek_record(fd, rec_offset, seek_base);
    return read(fd, out->name, sizeof(out->name)) +
        read(fd, &out->cnt, sizeof(out->cnt));
}

