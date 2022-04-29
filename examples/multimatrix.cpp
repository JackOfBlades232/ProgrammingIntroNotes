// The MultiMatrix template demo
// Copyright (c) Andrey Vikt. Stolyarov, 2011-2020
// This is just a sample program for a book devoted to C++, see
// http://www.stolyarov.info/books/cppintro for details.
// No rights restricted; do whatever you want with this code,
// if you really want to.

#include <stdio.h>

template <class T>
class Array {
    T *p;
    T init;
    unsigned int size;
public:
    Array(T in) : p(0), init(in), size(0) {}
    ~Array() { if(p) delete[] p; }
    T& operator[](unsigned int idx) {
        if(idx >= size) Resize(idx);
        return p[idx];
    }
    int Size() const { return size; }
private:
    void Resize(unsigned int required_index) {
        unsigned int new_size = size==0 ? 8 : size;
        while(new_size <= required_index)
            new_size *= 2;
        T *new_array = new T[new_size];
        for(unsigned int i = 0; i < new_size; i++)
            new_array[i] = i < size ? p[i] : init;
        if(p) delete[] p;
        p = new_array;
        size = new_size;
    }
    void operator=(const Array<T>& ref) {}
    Array(const Array<T>& ref) {}
};

template <class T, T init_val, int dim>
class MultiMatrix {
    Array<MultiMatrix<T, init_val, dim-1>*> arr;
public:
    MultiMatrix() : arr(0) {}
    ~MultiMatrix() {
        for(int i=0; i < arr.Size(); i++)
            if(arr[i]) delete arr[i];
    }
    MultiMatrix<T, init_val, dim-1>& operator[](unsigned int idx) {
        if(!arr[idx])
            arr[idx] = new MultiMatrix<T, init_val, dim-1>;
        return *arr[idx];
    }
};

#if 0

template <class T, T init_val>
class MultiMatrix<T, init_val, 1> {
    Array<T> arr;
public:
    MultiMatrix() : arr(init_val) {}
    T& operator[](unsigned int idx) {
        return arr[idx];
    }
};

#else

template <class T, T init_val>
class MultiMatrix<T, init_val, 1> : public Array<T> {
public:
    MultiMatrix() : Array<T>(init_val) {}
};

#endif



int main() {
    MultiMatrix<int, -1, 5> mm;
    mm[3][4][5][2][7] = 193;
    mm[2][2][2][2][2] = 251;
    printf("%d %d %d %d\n",
        mm[3][4][5][2][7],
        mm[2][2][2][2][2],
        mm[0][1][2][3][4],
        mm[1][2][3][2][1]);
    return 0;
}
