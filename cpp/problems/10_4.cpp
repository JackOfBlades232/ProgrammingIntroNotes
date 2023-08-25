/* 10_4.cpp */
#include <stdio.h>

class I {
    friend class M;

    int x, y;

public:
    I(int a_x, int a_y) : x(a_x), y(a_y) {}
};

class M {
    int matr[3][3];

public:
    M() {
        for (int i = 0; i < 3; i++)
            for (int j = 0; j < 3; j++)
                matr[i][j] = i == j ? 1 : 0;
    }

    int &operator[](I idx) { return matr[idx.y-1][idx.x-1]; }
    friend M operator+(const M &m1, const M &m2);
};

M operator+(const M &m1, const M &m2)
{
    M res;
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 3; j++)
            res.matr[i][j] = m1.matr[i][j] + m2.matr[i][j];
    return res;
}

int main()
{
    M m1;
    printf("%d %d %d\n", m1[I(1, 1)],  m1[I(2, 2)], m1[I(2, 3)]);
    M m2;
    m1[I(2, 3)] = 7;
    m2[I(2, 3)] = 350;
    M m3(m1 + m2);
    printf("%d %d %d\n", m3[I(1, 1)],  m3[I(2, 2)], m3[I(2, 3)]);
    return 0;
}
