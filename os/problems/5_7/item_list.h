/* 5_7/item_list.h */
#ifndef ITEM_LIST_SENTRY
#define ITEM_LIST_SENTRY

#include <stdio.h>

enum {
    item_name_size = 60
};

struct item_record {
    char name[item_name_size];
    int cnt;
};

int add_record(const char *filepath, const char *item_name);
int query_record(const char *filepath, const char *item_name);
int list_records(const char *filepath, FILE *out_f);

#endif
