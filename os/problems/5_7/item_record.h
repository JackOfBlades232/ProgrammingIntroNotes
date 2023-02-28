/* 5_7/item_record.h */
#ifndef ITEM_RECORD_SENTRY
#define ITEM_RECORD_SENTRY

enum {
    item_name_size = 60
};

struct item_record {
    char name[item_name_size];
    int cnt;
};

int write_record(int fd, int rec_offset, int seek_base,
        const struct item_record *item);
int read_record(int fd, int rec_offset, int seek_base, 
        struct item_record *out);

#endif
