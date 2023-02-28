/* 5_7/item_hashlist.c */
#include "item_list.h"
#include "item_record.h"
#include "strings.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

enum {
    hash_base = 0,
    hash_multiplier = 13
};

static int hashfunc(const char *name, int file_len)
{
    int hash = hash_base;
    const char *p;

    for (p = name; *p; p++) {
        hash = hash * hash_multiplier + *p;
        if (hash > file_len)
            hash -= file_len;
    }

    return hash;
}

static int fadd_record(int fd, int file_len, const char *item_name)
{
    int pos = hashfunc(item_name, file_len);
    struct item_record rec;
    int read_res = sizeof(rec);

    lseek(fd, pos, SEEK_SET);
    while ((read_res = read_record(fd, 0, SEEK_CUR, &rec)) == sizeof(rec)) {
        if (rec.cnt > 0 && strings_are_equal(rec.name, item_name))
            rec.cnt++;
        else if (rec.cnt == 0) {
            string_copy(item_name, rec.name, sizeof(rec.name));
            rec.cnt = 1;
        } else
            continue;

        return write_record(fd, -1, SEEK_CUR, &rec) == sizeof(rec);
    }
    
    return 0;
}

int add_record(const char *filepath, const char *item_name)
{
    int fd;
    int file_len;
    int res;

    fd = open(filepath, O_RDWR | O_CREAT,  0666);
    if (fd == -1)
        return 0;

    file_len = lseek(fd, 0, SEEK_END) / sizeof(struct item_record);
    res = fadd_record(fd, file_len, item_name);
    close(fd);
    return res;
}

int query_record(const char *filepath, const char *item_name);
int list_records(const char *filepath, FILE *out_f);

int join_lists(const char *in_filepath1, const char *in_filepath2, 
        const char *out_filepath);
