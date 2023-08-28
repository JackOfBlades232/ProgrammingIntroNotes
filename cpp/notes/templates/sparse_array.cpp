/* templates/sparse_array.cpp */
#include <stdio.h>

/*
 * Let us remake the class implemented in the adt chapter to be a template
 */

template <class T>
class SparseArray {
    struct item_t {
        int index;
        T value;
        item_t *next;
    };
    item_t *first;

public:
    SparseArray() : first(NULL) {}
    ~SparseArray();

    class Interm {
        friend class SparseArray<T>;

        SparseArray<T> *master;
        int index;
        
        Interm(SparseArray<T> *a_master, int idx)
            : master(a_master), index(idx) {}

        T &Provide();
        void Remove();

    public:
        operator T();
        T operator=(T x);
        T operator+=(T x);
        T operator-=(T x);
        T operator*=(T x);
        T operator/=(T x);
        T operator%=(T x);
        T operator&=(T x);
        T operator|=(T x);
        T operator^=(T x);
        T operator>>=(T x);
        T operator<<=(T x);
        T operator++();
        T operator--();
        T operator++(T);
        T operator--(T);
    };
    friend class Interm;

    Interm operator[](int idx)
        { return Interm(this, idx); }

private:
    SparseArray(const SparseArray<T>&) {}
    void operator=(const SparseArray<T>&) {}
};

// One could also specialize class templates:
template <>
class SparseArray<bool> {
    // New logic
};

int main()
{
    SparseArray<int> arr;
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

template <class T>
SparseArray<T>::~SparseArray()
{
    while (first) {
        item_t *tmp = first;
        first = first->next;
        delete tmp;
    }
}

template <class T>
SparseArray<T>::Interm::operator T()
{
    item_t *tmp;
    for (tmp = master->first; tmp; tmp = tmp->next) {
        if (tmp->index == index)
            return tmp->value;
    }
    return 0;
}

template <class T>
T SparseArray<T>::Interm::operator=(T x)
{
    if (x == 0)
        Remove();
    else
        Provide() = x;
    return x;
}

#define MOD_OP_TEXT(_op) \
    T &location = Provide(); \
    location _op x; \
    T res = location; \
    if (res == 0) \
        Remove(); \
    return res; \

template <class T> T SparseArray<T>::Interm::operator+=(T x) { MOD_OP_TEXT(+=); }
template <class T> T SparseArray<T>::Interm::operator-=(T x) { MOD_OP_TEXT(-=); }
template <class T> T SparseArray<T>::Interm::operator*=(T x) { MOD_OP_TEXT(*=); }
template <class T> T SparseArray<T>::Interm::operator/=(T x) { MOD_OP_TEXT(/=); }
template <class T> T SparseArray<T>::Interm::operator%=(T x) { MOD_OP_TEXT(%=); }
template <class T> T SparseArray<T>::Interm::operator&=(T x) { MOD_OP_TEXT(&=); }
template <class T> T SparseArray<T>::Interm::operator|=(T x) { MOD_OP_TEXT(|=); }
template <class T> T SparseArray<T>::Interm::operator^=(T x) { MOD_OP_TEXT(^=); }
template <class T> T SparseArray<T>::Interm::operator>>=(T x) { MOD_OP_TEXT(>>=); }
template <class T> T SparseArray<T>::Interm::operator<<=(T x) { MOD_OP_TEXT(<<=); }

template <class T>
T SparseArray<T>::Interm::operator++()
{
    T &location = Provide();
    ++location;
    T res = location;
    if (res == 0)
        Remove();
    return res;
}

template <class T>
T SparseArray<T>::Interm::operator--()
{
    T &location = Provide();
    --location;
    T res = location;
    if (res == 0)
        Remove();
    return res;
}

template <class T>
T SparseArray<T>::Interm::operator++(T)
{
    T &location = Provide();
    T res = location;
    location++;
    if (res == 0)
        Remove();
    return res;
}

template <class T>
T SparseArray<T>::Interm::operator--(T)
{
    T &location = Provide();
    T res = location;
    location--;
    if (res == 0)
        Remove();
    return res;
}

template <class T>
T &SparseArray<T>::Interm::Provide()
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

template <class T>
void SparseArray<T>::Interm::Remove()
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
