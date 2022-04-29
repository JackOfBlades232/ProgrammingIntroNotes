#include <stdio.h>
#include <stdlib.h>


struct item {
    int val;
    struct item *next;
};

typedef int (*intfunptr)(int, int);


#ifdef SHORT_VERS
int intlist_reduce_l(intfunptr f, int i, struct item *ls)
{
    return ls ? intlist_reduce_l(f, f(i, ls->val), ls->next) : i;
}
int intlist_reduce_r(intfunptr f, int i, struct item *ls)
{
    return ls ? f(ls->val, intlist_reduce_r(f, i, ls->next)) : i;
}
#else
int intlist_reduce_l(intfunptr f, int i, struct item *ls)
{
    int new_i;
    if(!ls)
        return i;
    new_i = f(i, ls->val);
    return intlist_reduce_l(f, new_i, ls->next);
}
int intlist_reduce_r(intfunptr f, int i, struct item *ls)
{
    int rest_res;
    if(!ls)
        return i;
    rest_res = intlist_reduce_r(f, i, ls->next);
    return f(ls->val, rest_res);
}
#endif

int int_plus(int x, int y) { return x + y; }
int int_mul(int x, int y) { return x * y; }
int int_max(int x, int y) { return x > y ? x : y; }
int int_zcnt_left(int n, int x) { return x==0 ? n+1 : n; }
int int_zcnt_right(int x, int n) { return x==0 ? n+1 : n; }


struct item* array2list(int *a, int n)
{
    struct item *p;
    if(!n)
        return NULL;
    p = malloc(sizeof(*p));
    p->val = *a;
    p->next = array2list(a+1, n-1);
    return p;
}

void freelist(struct item *p)
{
    if(!p)
        return;
    freelist(p->next);
    free(p);
}

int main()
{
    int s, p, ma, mb, z1, z2;

    int a[] = { 1,2,3,4,5,6,7,8 };
    int b[] = { 1,2,0,13,4,15,61,7,0,8 };

    struct item *lsa = array2list(a, 8);
    struct item *lsb = array2list(b, 10);

#ifdef SHORT_VERS
    printf("using short versions...\n");
#endif

    s = intlist_reduce_l(int_plus, 0, lsa);
    p = intlist_reduce_l(int_mul, 1, lsa);
    printf("%d %d\n", s, p);
    s = intlist_reduce_l(int_plus, 0, lsb);
    p = intlist_reduce_l(int_mul, 1, lsb);
    printf("%d %d\n", s, p);
    s = intlist_reduce_r(int_plus, 0, lsa);
    p = intlist_reduce_r(int_mul, 1, lsa);
    printf("%d %d\n", s, p);
    s = intlist_reduce_r(int_plus, 0, lsb);
    p = intlist_reduce_r(int_mul, 1, lsb);
    printf("%d %d\n", s, p);

    ma = intlist_reduce_l(int_max, lsa->val, lsa->next);
    mb = intlist_reduce_l(int_max, lsb->val, lsb->next);
    printf("%d %d\n", ma, mb);


    z1 = intlist_reduce_l(int_zcnt_left, 0, lsb);
    z2 = intlist_reduce_r(int_zcnt_right, 0, lsb);
    printf("%d %d\n", z1, z2);


    return 0;
}
