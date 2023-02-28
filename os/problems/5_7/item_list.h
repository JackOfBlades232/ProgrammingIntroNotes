/* 5_7/item_list.h */
#ifndef ITEM_LIST_SENTRY
#define ITEM_LIST_SENTRY

#include <stdio.h>

int add_record(const char *filepath, const char *item_name);
int query_record(const char *filepath, const char *item_name);
int list_records(const char *filepath, FILE *out_f);

int join_lists(const char *in_filepath1, const char *in_filepath2, 
        const char *out_filepath);

#endif
