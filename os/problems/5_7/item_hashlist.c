/* 5_7/item_hashlist.c */
#include "item_list.h"
#include "item_record.h"
#include "strings.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

enum {
    hash_base = 0,
    hash_multiplier = 13
};

enum {
    hashtable_num_records = 1048576
};

static int hashfunc(const char *name, int file_len)
{
    int hash = hash_base;
    const char *p;

    for (p = name; *p; p++) {
        hash = hash * hash_multiplier + *p;
        while (hash > file_len)
            hash -= file_len;
    }

    return hash;
}

static int get_record_file_len(int fd)
{
    return lseek(fd, 0, SEEK_END) / sizeof(struct item_record);
}

static int fadd_record(int fd, int file_len, const char *item_name, int amt)
{
    struct item_record rec;
    int read_res = sizeof(rec);
    int pos = hashfunc(item_name, file_len) * sizeof(rec);

    lseek(fd, pos, SEEK_SET);
    while ((read_res = read_record(fd, 0, SEEK_CUR, &rec)) == sizeof(rec)) {
        if (rec.cnt > 0 && strings_are_equal(rec.name, item_name))
            rec.cnt += amt;
        else if (rec.cnt == 0) {
            string_copy(item_name, rec.name, sizeof(rec.name));
            rec.cnt = amt;
        } else
            continue;

        return write_record(fd, -1, SEEK_CUR, &rec) == sizeof(rec);
    }

    if (read_res == 0) {
        string_copy(item_name, rec.name, sizeof(rec.name));
        rec.cnt = amt;
        return write_record(fd, 0, SEEK_END, &rec) == sizeof(rec);
    }
    
    return 0;
}

int add_record(const char *filepath, const char *item_name)
{
    int fd;
    int res;
    int file_len;

    fd = open(filepath, O_RDWR | O_CREAT, 0666);
    if (fd == -1)
        return 0;

    file_len = get_record_file_len(fd);
    if (file_len == 0) {
        ftruncate(fd, hashtable_num_records * sizeof(struct item_record));
        file_len = hashtable_num_records;
    }

    res = fadd_record(fd, file_len, item_name, 1);
    close(fd);
    return res;
}

static int fquery_record(int fd, int file_len, const char *item_name)
{
    struct item_record rec;
    int read_res = sizeof(rec);
    int pos = hashfunc(item_name, file_len) * sizeof(rec);

    lseek(fd, pos, SEEK_SET);
    while ((read_res = read_record(fd, 0, SEEK_CUR, &rec)) == sizeof(rec)) {
        if (rec.cnt > 0 && strings_are_equal(rec.name, item_name))
            return rec.cnt;
        else if (rec.cnt == 0)
            return 0;
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

    res = fquery_record(fd, get_record_file_len(fd), item_name);
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

    while ((read_res = read_record(fd, 0, SEEK_CUR, &rec)) == sizeof(rec)) {
        if (rec.cnt != 0)
            fprintf(out_f, "%s: %d\n", rec.name, rec.cnt);
    }

    close(fd);
    return read_res == 0;
}

int join_lists(const char *in_filepath1, const char *in_filepath2, 
        const char *out_filepath)
{
    int in_fd1 = -1, in_fd2 = -1, out_fd = -1;
    struct item_record rec;
    int read_res = sizeof(rec);

    int infile_len1, infile_len2;
    int q_res;
    
    in_fd1 = open(in_filepath1, O_RDONLY);
    if (in_fd1 == -1)
        goto join_error;

    in_fd2 = open(in_filepath2, O_RDONLY);
    if (in_fd2 == -1)
        goto join_error;

    out_fd = open(out_filepath, O_RDWR | O_CREAT | O_TRUNC, 0666);
    if (out_fd == -1)
        goto join_error;

    infile_len1 = get_record_file_len(in_fd1);
    infile_len2 = get_record_file_len(in_fd2);

    ftruncate(out_fd, hashtable_num_records * sizeof(struct item_record));

    lseek(in_fd1, 0, SEEK_SET);
    while ((read_res = read_record(in_fd1, 0, SEEK_CUR, &rec)) == sizeof(rec)) {
        if (rec.cnt == 0)
            continue;

        q_res = fquery_record(in_fd2, infile_len2, rec.name);
        if (q_res == -1)
            goto join_error;

        fadd_record(out_fd, hashtable_num_records, rec.name, rec.cnt + q_res);
    }

    lseek(in_fd2, 0, SEEK_SET);
    while ((read_res = read_record(in_fd2, 0, SEEK_CUR, &rec)) == sizeof(rec)) {
        if (rec.cnt == 0)
            continue;

        q_res = fquery_record(in_fd1, infile_len1, rec.name);
        if (q_res == -1)
            goto join_error;

        if (q_res == 0) {
            fadd_record(out_fd, hashtable_num_records, rec.name, rec.cnt);
        }
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
