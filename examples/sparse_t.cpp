// The SparseArray template demo
// Copyright (c) Andrey Vikt. Stolyarov, 2010-2021
// This is just a sample program for a book devoted to C++, see
// http://www.stolyarov.info/books/cppintro for details.
// No rights restricted; do whatever you want with this code,
// if you really want to.


#include <stdio.h>

template<class T>
class SparseArray {
    struct Item {
        int index;
        T value;
        Item *next;
    };
    Item *first;
public:
    SparseArray() : first(0) {}
    ~SparseArray();
    class Interm {
        friend class SparseArray<T>;
        SparseArray<T> *master;
        int index;
        Interm(SparseArray<T> *a_master, int ind)
            : master(a_master), index(ind) {}
        T& Provide();
        void Remove();
    public:
        operator T();
        T operator=(T x);
        T operator+=(T x);
        T operator++();
        T operator++(int);
    };
    friend class Interm;

    Interm operator[](int idx)
        { return Interm(this, idx); }

    int NonzeroCount() const;

private:
    SparseArray(const SparseArray<T>&) {}
    void operator=(const SparseArray<T>&) {}
};

template <class T>
SparseArray<T>::~SparseArray()
{
    while(first) {
        Item *tmp = first;
        first = first->next;
        delete tmp;
    }
}

template <class T>
int SparseArray<T>::NonzeroCount() const
{
    int r = 0;
    for(Item *tmp = first; tmp; tmp = tmp->next)
        r++;
    return r;
}

template <class T>
T& SparseArray<T>::Interm::Provide()
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

template <class T>
void SparseArray<T>::Interm::Remove()
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

template <class T>
T SparseArray<T>::Interm::operator=(T x)
{
    if(x == 0)
        Remove();
    else
        Provide() = x;
    return x;
}

template <class T>
T SparseArray<T>::Interm::operator+=(T x)
{
    T& location = Provide();
    location += x;
    T res = location;
    if(res == 0)
        Remove();
    return res;
}

template <class T>
T SparseArray<T>::Interm::operator++()
{
    T& location = Provide();
    T res = ++location;
    if(location == 0)
        Remove();
    return res;
}

template <class T>
T SparseArray<T>::Interm::operator++(int)
{
    T& location = Provide();
    T res = location++;
    if(location == 0)
        Remove();
    return res;
}

template <class T>
SparseArray<T>::Interm::operator T()
{
    Item* tmp;
    for(tmp = master->first; tmp; tmp = tmp->next) {
        if(tmp->index == index) {
            return tmp->value;
        }
    }
    return 0;
}

static void do_query(SparseArray<int> &array, char *str)
{
    int idx;
    if(1 != sscanf(str, "%d", &idx)) {
        printf("Invalid index\n");
        return;
    }
    printf("** array[%d] == %d\n", idx, (int) (array[idx]));
}

static void do_assign(SparseArray<int> &array, char *str)
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
    SparseArray<int> array;
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
