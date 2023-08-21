/* paradigms/some_recursive_stuff.c */
#include <stdio.h>
#include <stdlib.h>

/* simple recursion: the function calls itself exactly once */
int array_sum(const int arr[], int len)
{
    return len <= 0 ? 0 : *arr + array_sum(arr+1, len-1);
}

struct item {
    int val;
    struct item *next;
};

typedef int (*intfuncptr)(int, int);

/* Reductions: left : f(f(...f(v, a1), a2) .. ), an)
               right: f(a1, f(a2, ... , f(an, v)))..)
    Where v is the dummy init elem */

int intlist_reduce_l(intfuncptr f, int i, struct item *ls)
{
    return ls ? intlist_reduce_l(f, f(i, ls->val), ls->next) : i;
}

int intlist_reduce_r(intfuncptr f, int i, struct item *ls)
{
    return ls ? f(ls->val, intlist_reduce_r(f, i, ls->next)) : i;
}

int int_plus(int x, int y) { return x + y; }
int int_mul(int x, int y) { return x * y; }
int int_max(int x, int y) { return x > y ? x : y; }
int int_zcnt_left(int n, int x) { return x == 0 ? n+1 : n; }
int int_zcnt_right(int x, int n) { return x == 0 ? n+1 : n; }

/* this is a tail recursion, thus it can be easily rewritten as a cycle
 * for better performance. In some languages this optimization is done
 * under the hood */
const struct item *find_value(const struct item *lst, int val)
{
    if (!lst)
        return NULL;
    if (lst->val == val)
        return lst;
    return find_value(lst->next, val);
}

/* accamulator param demonstration */
int list_length_do(const struct item *lst, int cnt)
{
    return lst ? list_length_do(lst->next, cnt+1) : cnt;
}
int list_length_acc(const struct item *lst) { return list_length_do(lst, 0); }

int array_sum_do(int *arr, int len, int sum)
{
    return len > 0 ? array_sum_do(arr+1, len-1, *arr + sum) : sum;
}
int array_sum_acc(int *arr, int len) { return array_sum_do(arr, len, 0); }

struct item *reverse_list_do(struct item *lst, struct item *res)
{
    struct item *list_rest;
    if (!lst)
        return res;
    list_rest = lst->next;
    lst->next = res;
    return reverse_list_do(list_rest, lst);
}
struct item *reverse_list_acc(struct item *lst) 
{
    return reverse_list_do(lst, NULL); 
}

/* this thing counts the str length in recursive calls, 
 * malloc's the copy at the bootom and copies the characters
 * end to start as it unrolls */
char *mad_strdup_do(const char *str, int depth)
{
    char *res;
    if (*str)
        res = mad_strdup_do(str+1, depth+1);
    else
        res = malloc(depth+1);
    res[depth] = *str;
    return res;
}
char *mad_strdup(const char *str) { return mad_strdup_do(str, 0); }

int main()
{
    int arr[] = { 1, 3, 5, 7, 9, -1 };
    printf("Arr sum (rec): %d\n", array_sum(arr, sizeof(arr)/sizeof(*arr)));
    printf("Arr sum (acc): %d\n", array_sum_acc(arr, sizeof(arr)/sizeof(*arr)));

    struct item *ls;
    struct item *p = NULL;
    for (int i = 0; i < sizeof(arr)/sizeof(*arr); i++) {
        struct item *tmp = malloc(sizeof(*tmp));
        tmp->val = arr[i];
        tmp->next = NULL;
        if (p)
            p->next = tmp;
        else
            ls = tmp;
        p = tmp;
    }

    printf("List sum (red): %d\n", intlist_reduce_l(int_plus, 0, ls));
    printf("List prod (red): %d\n", intlist_reduce_l(int_mul, 1, ls));
    printf("List max (red): %d\n", intlist_reduce_l(int_max, ls->val, ls)); 
    printf("List left zcnt (red): %d\n", intlist_reduce_l(int_zcnt_left, 0, ls)); 
    printf("List right zcnt (red): %d\n", intlist_reduce_r(int_zcnt_right, 0, ls)); 

    printf("List len (acc): %d\n", list_length_acc(ls));

    printf("Input value to query: ");
    int val;
    if (scanf("%d", &val) != 1) {
        fprintf(stderr, "Invalid input!");
        return 1;
    }

    printf(find_value(ls, val) ? "Found\n" : "Not found\n");

    ls = reverse_list_acc(ls);
    printf("Reversed list:");
    for (struct item *tmp = ls; tmp; tmp = tmp->next)
        printf(" %d", tmp->val);
    putchar('\n');

    const char str[] = "Hiyah";
    printf("String: %s, Dupped string (mad recursion): %s\n", 
           str, mad_strdup(str));
    /* let the OS free the thing */

    return 0;
}
