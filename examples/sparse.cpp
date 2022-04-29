// The SparseArray demo
// Copyright (c) Andrey Vikt. Stolyarov, 2010-2020
// This is just a sample program for a book devoted to C++, see
// http://www.stolyarov.info/books/cppintro for details.
// No rights restricted; do whatever you want with this code,
// if you really want to.


#include <stdio.h>

class SparseArrayInt {
    struct Item {
        int index;
        int value;
        Item *next;
    };
    Item *first;
public:
    SparseArrayInt() : first(0) {}
    ~SparseArrayInt();
    class Interm {
        friend class SparseArrayInt;
        SparseArrayInt *master;
        int index;
        Interm(SparseArrayInt *a_master, int ind)
            : master(a_master), index(ind) {}
        int& Provide();
        void Remove();
    public:
        operator int();
        int operator=(int x);
        int operator+=(int x);
        int operator++();
        int operator++(int);
    };
    friend class Interm;

    Interm operator[](int idx)
        { return Interm(this, idx); }

    int NonzeroCount() const;

private:
    SparseArrayInt(const SparseArrayInt&) {}
    void operator=(const SparseArrayInt&) {}
};

SparseArrayInt::~SparseArrayInt()
{
    while(first) {
        Item *tmp = first;
        first = first->next;
        delete tmp;
    }
}

int SparseArrayInt::NonzeroCount() const
{
    int r = 0;
    for(Item *tmp = first; tmp; tmp = tmp->next)
        r++;
    return r;
}

int& SparseArrayInt::Interm::Provide()
{
    Item* tmp;
    for(tmp = master->first; tmp; tmp = tmp->next) {
        if(tmp->index == index)
            return tmp->value;
    }
    tmp = new Item;
    tmp->index = index;
    tmp->next = master->first;
    master->first = tmp;
    return tmp->value;
}

void SparseArrayInt::Interm::Remove()
{
    Item** tmp;
    for(tmp = &(master->first); *tmp; tmp = &(*tmp)->next) {
        if((*tmp)->index == index) {
            Item *to_delete = *tmp;
            *tmp = (*tmp)->next;
            delete to_delete;
            return;
        }
    }
}

int SparseArrayInt::Interm::operator=(int x)
{
    if(x == 0)
        Remove();
    else
        Provide() = x;
    return x;
}

int SparseArrayInt::Interm::operator+=(int x)
{
    int& location = Provide();
    location += x;
    int res = location;
    if(res == 0)
        Remove();
    return res;
}

int SparseArrayInt::Interm::operator++()
{
    int& location = Provide();
    int res = ++location;
    if(location == 0)
        Remove();
    return res;
}

int SparseArrayInt::Interm::operator++(int)
{
    int& location = Provide();
    int res = location++;
    if(location == 0)
        Remove();
    return res;
}

SparseArrayInt::Interm::operator int()
{
    Item* tmp;
    for(tmp = master->first; tmp; tmp = tmp->next) {
        if(tmp->index == index) {
            return tmp->value;
        }
    }
    return 0;
}

static void do_query(SparseArrayInt &array, char *str)
{
    int idx;
    if(1 != sscanf(str, "%d", &idx)) {
        printf("Invalid index\n");
        return;
    }
    printf("** array[%d] == %d\n", idx, (int) (array[idx]));
}

static void do_assign(SparseArrayInt &array, char *str)
{
    int idx, val;
    if(2 != sscanf(str, "%d %d", &idx, &val)) {
        printf("Invalid numbers\n");
        return;
    }
    array[idx] = val;
}

int main()
{
    char buf[1024];
    SparseArrayInt array;
    while(fgets(buf, sizeof(buf), stdin)) {
        switch(buf[0]) {
            case '?': do_query(array, buf+1); break;
            case '!': do_assign(array, buf+1); break;
            case '#':
                printf("%d items now\n", array.NonzeroCount());
                break;
            case 'q': return 0;
            default:
                printf("Unknown action, must be '?' or '!'\n");
        }
    }
    return 0;
}
