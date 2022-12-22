/* 4_22/test_generation/generate_test.c */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

enum { min_elements = 10, max_elements = 150, min_char = 33, max_char = 126 };

int random_in_range(int min, int max)
{
    return min + (int)(((double)max + 1) * rand() / (RAND_MAX + 1.0));
}

int main() 
{
    int num_elements;

    srand(time(NULL));
    num_elements = random_in_range(min_elements, max_elements);

    for (; num_elements > 0; num_elements--) {
        printf("a%c\n", random_in_range(min_char, max_char));
    }

    printf("c1\n");

    return 0;
}
