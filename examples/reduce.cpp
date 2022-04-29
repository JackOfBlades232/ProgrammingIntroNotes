#include <stdio.h>


template <class item, class elem>
class ListPosition {
    item *p;
public:
    ListPosition(item *lst) : p(lst) {}
    ~ListPosition() {}
    ListPosition<item, elem> Next() const
        { return ListPosition(p->next); }
    bool Empty() const { return !p; }
    const elem &Get() const { return p->data; }
};

template <class elem>
class ArrayPosition {
    elem *p;
    int rest_size;
public:
    ArrayPosition(elem *a, int len) : p(a), rest_size(len) {}
    ~ArrayPosition() {}
    ArrayPosition<elem> Next() const
        { return ArrayPosition(p+1, rest_size-1); }
    bool Empty() const { return rest_size < 1; }
    const elem &Get() const { return *p; }
};

class InputStreamPosition {
    FILE *f;
    int c;
public:
    InputStreamPosition(FILE *af) : f(af) { c = fgetc(af); }
    ~InputStreamPosition() {}
    InputStreamPosition Next() const { return InputStreamPosition(f); }
    bool Empty() const { return c == EOF || c == '\n'; }
    int Get() const { return c; }
};

template <class Pos, class Elem, class Res>
Res reduce_right(Res (*func)(Elem, Res), Pos pos, Res init)
{
    return pos.Empty() ? init :
        func(pos.Get(), reduce_right(func, pos.Next(), init));
}

template <class Pos, class Elem, class Res>
Res reduce_left(Res (*func)(Elem, Res), Pos pos, Res init)
{
    return pos.Empty() ? init :
        reduce_left(func, pos.Next(), func(pos.Get(), init));
}

template <class t> t sum(t x, t y) { return x + y; }
template <class t> t prod(t x, t y) { return x * y; }


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

void print_ilist(const struct int_item *list)
{
    if(!list) {
        printf("\n");
        return;
    }
    printf("%d ", list->data);
    print_ilist(list->next);
}

struct int_item *print_il(int d, struct int_item *rest)
{
    printf("%d ", d);
    return 0;
}

int main()
{
    {
        int array[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        int res1 =
            reduce_left(sum<int>, ArrayPosition<int>(array, 10), 0);
        int res2 =
            reduce_right(sum<int>, ArrayPosition<int>(array, 10), 0);
        printf("%d %d\n", res1, res2);

        int res3 =
            reduce_left(prod<int>, ArrayPosition<int>(array, 10), 1);
        int res4 =
            reduce_right(prod<int>, ArrayPosition<int>(array, 10), 1);
        printf("%d %d\n", res3, res4);

        struct int_item * list =
            reduce_right(make_il, ArrayPosition<int>(array, 10), (int_item*)0);

        print_ilist(list);

        int res5 =
            reduce_left(prod<int>, ListPosition<int_item, int>(list), 1);
        printf("%d\n", res5);

        reduce_left(print_il, ListPosition<int_item, int>(list), (int_item*)0);
        printf("\n");
        reduce_right(print_il, ListPosition<int_item, int>(list), (int_item*)0);
        printf("\n");

    }

    return 0;
}
