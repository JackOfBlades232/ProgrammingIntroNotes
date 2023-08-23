/* adt/int_array.cpp */
#include <stdio.h>

class IntArray {
    int *p;
    unsigned int size;

public:
    IntArray() {
        size = 16;
        p = new int[size];
    }
    ~IntArray() { delete [] p; }
    int &operator[](unsigned int idx); // can only be a method, not outer func

private:
    void Resize(unsigned int required_index);

    // Restrict copies and assignments
    void operator=(const IntArray &ref) {}
    IntArray(const IntArray &ref) {}
};

int &IntArray::operator[](unsigned int idx)
{
    if (idx >= size)
        Resize(idx);
    return p[idx];
}

void IntArray::Resize(unsigned int required_index)
{
    unsigned int new_size = size;
    while (new_size <= required_index)
        new_size *= 2;
    int *new_p = new int[new_size];
    for (unsigned int i = 0; i < size; i++)
        new_p[i] = p[i];
    delete [] p;
    p = new_p;
    size = new_size;
}

int main()
{
    IntArray arr;
    arr[500] = 15;
    arr[1000] = 30;
    arr[10] = arr[500] + 1;
    arr[10]++;

    for (int i = 0; i < 1001; i++) {
        if (arr[i])
            printf("(%d, %d) ", i, arr[i]);
    }
    putchar('\n');
    return 0;
}
