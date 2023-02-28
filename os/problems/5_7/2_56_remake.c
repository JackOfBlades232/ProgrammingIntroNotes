/* 5_7/2_56_remake.c */
#include "item_list.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    if (argc < 4) {
        fprintf(stderr, "Provide 2 filepaths for join and out filepath\n");
        return 1;
    }

    return outer_join(argv[1], argv[2], argv[3]) ? 0 : 2;
}
