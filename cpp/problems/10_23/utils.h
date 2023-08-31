/* 10_23/utils.h */
#ifndef UTILS_SENTRY
#define UTILS_SENTRY

#include <stdlib.h>

#define ASSERT(_e, _fmt, ...) \
    if(!(_e)) { \
        fprintf(stderr, \
                "Assertion failed at %s:%d : " _fmt "\n", \
                __FILE__, __LINE__, ##__VA_ARGS__); \
        exit(1); \
    }

#define ABS(_a) ((_a) < 0 ? -(_a) : (_a))

static inline int randint(int min, int max)
{
    return min + (int)((float)(max-min+1) * rand() / (RAND_MAX+1.0));
}

void generate_even_permutation(int *perm, int size);
bool permutation_is_id(int *perm, int size);
bool permutation_is_odd(int *perm, int size);
void shuffle_permutation(int *perm, int size);

#endif
