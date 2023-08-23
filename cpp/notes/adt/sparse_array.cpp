/* adt/sparse_array.cpp */
#include <stdio.h>

/*
 * This a class for emulating a large sparse array via a list.
 * The indexing operator will return a special object, that will
 * do different things depending on whether an element exists or not.
 */

class SparseArrayInt {
    struct item_t {
        int index;
        int value;
        item_t *next;
    };
    item_t *first;

public:
    SparseArrayInt() : first(NULL) {}
    ~SparseArrayInt();

    class Interm {
        friend class SparseArrayInt;

        SparseArrayInt *master;
        int index;
        
        Interm(SparseArrayInt *a_master, int idx)
            : master(a_master), index(idx) {}

        int &Provide();
        void Remove();

    public:
        operator int();
        int operator=(int x);
        int operator+=(int x);
        int operator-=(int x);
        int operator*=(int x);
        int operator/=(int x);
        int operator%=(int x);
        int operator&=(int x);
        int operator|=(int x);
        int operator^=(int x);
        int operator>>=(int x);
        int operator<<=(int x);
        int operator++();
        int operator--();
        int operator++(int);
        int operator--(int);
    };
    friend class Interm;

    Interm operator[](int idx)
        { return Interm(this, idx); }

private:
    SparseArrayInt(const SparseArrayInt &) {}
    void operator=(const SparseArrayInt &) {}
};

int main()
{
    SparseArrayInt arr;
    int x;
    x = arr[500];
    arr[300] = 50;
    arr[200] = 10;
    arr[15] += 100;
    int y = arr[200]++;
    int z = --arr[300];
    printf("%d %d %d | %d %d %d %d\n", x, y, z, 
           (int)arr[15], (int)arr[200], (int)arr[300], (int)arr[500]);

    return 0;
}

SparseArrayInt::~SparseArrayInt()
{
    while (first) {
        item_t *tmp = first;
        first = first->next;
        delete tmp;
    }
}

SparseArrayInt::Interm::operator int()
{
    item_t *tmp;
    for (tmp = master->first; tmp; tmp = tmp->next) {
        if (tmp->index == index)
            return tmp->value;
    }
    return 0;
}

int SparseArrayInt::Interm::operator=(int x)
{
    if (x == 0)
        Remove();
    else
        Provide() = x;
    return x;
}

#define MODEQ_OP_TEXT(_op) \
    int &location = Provide(); \
    location _op x; \
    int res = location; \
    if (res == 0) \
        Remove(); \
    return res; \

int SparseArrayInt::Interm::operator+=(int x) { MODEQ_OP_TEXT(+=); }
int SparseArrayInt::Interm::operator-=(int x) { MODEQ_OP_TEXT(-=); }
int SparseArrayInt::Interm::operator*=(int x) { MODEQ_OP_TEXT(*=); }
int SparseArrayInt::Interm::operator/=(int x) { MODEQ_OP_TEXT(/=); }
int SparseArrayInt::Interm::operator%=(int x) { MODEQ_OP_TEXT(%=); }
int SparseArrayInt::Interm::operator&=(int x) { MODEQ_OP_TEXT(&=); }
int SparseArrayInt::Interm::operator|=(int x) { MODEQ_OP_TEXT(|=); }
int SparseArrayInt::Interm::operator^=(int x) { MODEQ_OP_TEXT(^=); }
int SparseArrayInt::Interm::operator>>=(int x) { MODEQ_OP_TEXT(>>=); }
int SparseArrayInt::Interm::operator<<=(int x) { MODEQ_OP_TEXT(<<=); }

int SparseArrayInt::Interm::operator++()
{
    int &location = Provide();
    ++location;
    int res = location;
    if (res == 0)
        Remove();
    return res;
}

int SparseArrayInt::Interm::operator--()
{
    int &location = Provide();
    --location;
    int res = location;
    if (res == 0)
        Remove();
    return res;
}

int SparseArrayInt::Interm::operator++(int)
{
    int &location = Provide();
    int res = location;
    location++;
    if (res == 0)
        Remove();
    return res;
}

int SparseArrayInt::Interm::operator--(int)
{
    int &location = Provide();
    int res = location;
    location--;
    if (res == 0)
        Remove();
    return res;
}

int &SparseArrayInt::Interm::Provide()
{
    item_t *tmp;
    for (tmp = master->first; tmp; tmp = tmp->next) {
        if (tmp->index == index)
            return tmp->value;
    }
    tmp = new item_t;
    tmp->index = index;
    tmp->next = master->first;
    master->first = tmp;
    return tmp->value;
}

void SparseArrayInt::Interm::Remove()
{
    item_t **tmp;
    for (tmp = &(master->first); *tmp; tmp = &(*tmp)->next) {
        if ((*tmp)->index == index) {
            item_t *to_delete = *tmp;
            *tmp = (*tmp)->next;
            delete to_delete;
            return;
        }
    }
}
