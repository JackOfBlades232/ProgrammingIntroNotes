/* templates/const_templates.cpp */
#include <stdio.h>

// Templates can also take typed constants as parameters, which enables
// new interesting stuff, like using various dimensions for static arrays
//
// These params may also be some specific pointers (like &var and &f for
// global variables and functions, i.e. the address should be known at 
// comptime), but this is kinda exotic

// Here is a slightly different example

template <class T>
class Array {
    T *p;
    T init;
    unsigned int size;

public:
    Array(T in) : p(NULL), init(in), size(0) {}
    ~Array() { if (p) delete[] p; }
    T &operator[](unsigned int idx) {
        if (idx >= size) Resize(idx);
        return p[idx];
    }
    unsigned int Size() const { return size; }

private:
    void Resize(unsigned int req_size) {
        unsigned int new_size = size == 0 ? 8 : size;
        while (new_size <= req_size)
            new_size *= 2;
        T *new_array = new T[new_size];
        for (unsigned int i = 0; i < new_size; i++)
            new_array[i] = i < size ? p[i] : init;
        if (p) delete[] p;
        p = new_array;
        size = new_size;
    }

    void operator=(const Array<T> &ref) {}
    Array(const Array<T> &ref) {}
};

// Now this class is an implementation of a multidim matrix, where 
// dimensions are a template val
template <class T, T init_val, int dim>
class MultiMatrix {
    Array<MultiMatrix<T, init_val, dim-1> *> arr;

public:
    MultiMatrix() : arr(NULL) {}
    ~MultiMatrix() {
        for (unsigned int i = 0; i < arr.Size(); i++)
            if (arr[i]) delete arr[i];
    }
    MultiMatrix<T, init_val, dim-1> &operator[](unsigned int idx) {
        if (!arr[idx])
            arr[idx] = new MultiMatrix<T, init_val, dim-1>;
        return *arr[idx];
    }
};

// Now, we have to specify the dim=1 case, since the regular MultiMatrix
// is defined recursively and depends on matrices of lesser dimensions
// This matrix is basically just an array with known init_val
template <class T, T init_val>
class MultiMatrix<T, init_val, 1> : public Array<T> {
public:
    MultiMatrix() : Array<T>(init_val) {}
};

int main()
{
    MultiMatrix<int, -1, 5> mm;
    mm[3][4][5][2][7] = 193;
    mm[2][2][2][2][2] = 251;
    printf("%d %d %d %d\n",
           mm[3][4][5][2][7], mm[2][2][2][2][2],
           mm[0][1][2][3][4], mm[1][2][3][2][1]);
    return 0;
}
