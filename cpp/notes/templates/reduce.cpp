/* templates/reduce.cpp */
#include <stdio.h>

/*
 * Reduction method from the recursion paradigm implemented
 * as templates for different types of sequences 
 * This could also have been done with an interface
 */

template <class TItem, class TElem>
class ListPosition {
    TItem *p;
public:
    ListPosition(TItem *lst) : p(lst) {}
    ~ListPosition() {}
    ListPosition<TItem, TElem> Next() const
        { return ListPosition(p->next); }
    bool Empty() const { return !p; }
    const TElem &Get() const { return p->data; }
};

template <class TElem>
class ArrayPosition {
    TElem *p;
    int rem_size;
public:
    ArrayPosition(TElem *arr, int size) : p(arr), rem_size(size) {}
    ~ArrayPosition() {}
    ArrayPosition<TElem> Next() const
        { return ArrayPosition(p+1, rem_size-1); }
    bool Empty() const { return rem_size <= 0; }
    const TElem &Get() const { return *p; }
};

template <class TPos, class TElem, class TRes>
TRes reduce_right(TRes (*func)(TElem, TRes), TPos pos, TRes init)
{
    return pos.Empty() ? init :
        func(pos.Get(), reduce_right(func, pos.Next(), init));
}

template <class TPos, class TElem, class TRes>
TRes reduce_left(TRes (*func)(TElem, TRes), TPos pos, TRes init)
{
    return pos.Empty() ? init :
        reduce_left(func, pos.Next(), func(pos.Get(), init));
}

template <class T>
T sum(T x, T y)
{
    return x + y;
}

template <class T>
T prod(T x, T y)
{
    return x * y;
}

// For using ListPosition
struct int_item {
    int data;
    struct int_item *next;
};

struct int_item *make_il(int d, struct int_item *rest)
{
    struct int_item *p = new int_item;
    p->data = d;
    p->next = rest;
    return p;
}

struct int_item *print_il(int d, struct int_item *rest)
{
    printf("%d ", d);
    return NULL;
}

int main()
{
    int arr[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    
    int res1 = reduce_left(sum<int>, ArrayPosition<int>(arr, 10), 0);
    int res2 = reduce_right(sum<int>, ArrayPosition<int>(arr, 10), 0);
    int res3 = reduce_left(prod<int>, ArrayPosition<int>(arr, 10), 1);
    int res4 = reduce_right(prod<int>, ArrayPosition<int>(arr, 10), 1);

    printf("array:");
    for (int i = 0; i < 10; i++)
        printf(" %d", arr[i]);
    printf("\nleft-sum: %d, right-sum: %d; left-prod: %d; right-prod: %d\n",
           res1, res2, res3, res4);

    // Making list from array using reduction
    // (int_item *) had to be specified, or the templade would not be defined
    struct int_item *list = 
        reduce_right(make_il, ArrayPosition<int>(arr, 10), (int_item *) NULL);

    // Now we can use the same reductions on list
    int res5 = reduce_left(prod<int>, ListPosition<int_item, int>(list), 1);
    printf("right-prod (list): %d\n", res5);

    // We can even go non-functional and use reduction with side-effects
    // In order
    reduce_left(print_il, ListPosition<int_item, int>(list), 
                (int_item *) NULL);
    putchar('\n');
    // Reverse order
    reduce_right(print_il, ListPosition<int_item, int>(list), 
                 (int_item *) NULL);
    putchar('\n');

    // Reduction can also be used to make a copy of the list, and in reverse
    // order, and we could add filtering, mutation and so on

    // This is not extremely practical, but neatly demonstrates the reduction
    // technique and the power of templates

    return 0;
}
