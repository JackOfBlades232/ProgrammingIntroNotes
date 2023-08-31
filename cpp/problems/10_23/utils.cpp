/* 10_23/utils.cpp */
#include "utils.h"

void generate_even_permutation(int *perm, int size)
{
    for (int i = 0; i < size; i++)
        perm[i] = i;

    while (
            permutation_is_id(perm, size) || 
            permutation_is_odd(perm, size)
          )
    {
        shuffle_permutation(perm, size);
    }
}

bool permutation_is_id(int *perm, int size)
{
    for (int i = 0; i < size; i++) {
        if (perm[i] != i)
            return false;
    }

    return true;
}

bool permutation_is_odd(int *perm, int size)
{
    // Since the array is 15 el long, it's ok to use the O(n^2) inv counting
    int inv_cnt = 0;
    for (int i = 0; i < size-1; i++)
        for (int j = i+1; j < size; j++) {
            if (perm[i] > perm[j])
                inv_cnt++;
        }
            
    return inv_cnt % 2 == 1;
}

void shuffle_permutation(int *perm, int size)
{
    for (int i = 0; i < size-1; i++) {
        int xchg_idx = randint(i, size-1);
        int tmp = perm[i];
        perm[i] = perm[xchg_idx];
        perm[xchg_idx] = tmp;
    }
}
