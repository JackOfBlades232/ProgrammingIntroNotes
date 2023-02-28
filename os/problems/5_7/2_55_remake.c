/* 5_7/2_55_remake.c */
#include "item_list.h"
#include "strings.h"

#include <stdio.h>
#include <stdlib.h>

void raise_argc_err()
{
    fprintf(stderr,
            "Provide file name, command and (for add/query) item name\n");
    exit(1);
}

int handle_add_cmd(int argc, char **argv)
{
    if (argc < 4)
        raise_argc_err();

    return add_record(argv[1], argv[3]) ? 0 : 2;
}

int handle_query_cmd(int argc, char **argv)
{
    int cnt;
    if (argc < 4)
        raise_argc_err();

    cnt = query_record(argv[1], argv[3]);
    if (cnt >= 0) {
        printf("%d\n", cnt);
        return 0;
    }

    return 2;
}

int handle_list_cmd(char **argv)
{
    int res = list_records(argv[1], stdout);
    return res ? 0 : 2;
}

int main(int argc, char **argv)
{
    if (argc < 3)
        raise_argc_err();

    if (strings_are_equal(argv[2], "add"))
        return handle_add_cmd(argc, argv);
    else if (strings_are_equal(argv[2], "query"))
        return handle_query_cmd(argc, argv);
    else if (strings_are_equal(argv[2], "list"))
        return handle_list_cmd(argv);

    fprintf(stderr, "Command (second arg) must be add, query or list\n");
    return 3;
}
