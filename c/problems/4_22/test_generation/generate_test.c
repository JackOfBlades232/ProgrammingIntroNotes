/* 4_22/test_generation/generate_test.c */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

enum { min_commands = 10, max_commands = 150, min_char = 33, max_char = 126 };

int random_in_range(int min, int max)
{
    return min + (int)(((double)max + 1) * rand() / (RAND_MAX + 1.0));
}

int main(int argc, char **argv) 
{
    int seed_param, num_commands;

    if (argc > 2) {
        fprintf(stderr, "Provide one arg, a random seed parameter, "
                        "or no args\n");
        return 1;
    }

    seed_param = argc == 2 ? atoi(argv[1]) : 0;
    srand(time(NULL) + seed_param);

    num_commands = random_in_range(min_commands, max_commands);

    for (; num_commands > 0; num_commands--) {
        int command;

        command = random_in_range(0, 1) ? 'a' : 'd';
        printf("%c%c\n", command, random_in_range(min_char, max_char));
    }

    printf("c_\n");

    return 0;
}
