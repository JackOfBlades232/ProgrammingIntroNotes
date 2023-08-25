/* 10_5.cpp */
#include <stdio.h>

class M {
    class C {
        friend class M;
        int col[3];
    public:
        int &operator[](int y) { return col[y-1]; }
        friend M operator+(const M &m1, const M &m2);
    };

    C matr[3];

public:
    M() {
        for (int i = 0; i < 3; i++)
            for (int j = 0; j < 3; j++)
                matr[i].col[j] = i == j ? 1 : 0;
    }

    C &operator[](int x) { return matr[x-1]; }
    friend M operator+(const M &m1, const M &m2);
};

M operator+(const M &m1, const M &m2)
{
    M res;
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < 3; j++)
            res.matr[i].col[j] = m1.matr[i].col[j] + m2.matr[i].col[j];
    return res;
}

int main()
{
    M m1;
    printf("%d %d %d\n", m1[1][1],  m1[2][2], m1[2][3]);
    M m2;
    m1[2][3] = 7;
    m2[2][3] = 350;
    M m3(m1 + m2);
    printf("%d %d %d\n", m3[1][1],  m3[2][2], m3[2][3]);
    return 0;
}

