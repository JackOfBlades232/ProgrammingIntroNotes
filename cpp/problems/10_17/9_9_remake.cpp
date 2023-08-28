/* 10_17/9_9_remake.cpp */
#include <stdio.h>

class FibFSM {
    int n1, n2;
    unsigned int cur_idx;

public:
    FibFSM() : n1(0), n2(1), cur_idx(0) {}
    int Step();
};

int FibFSM::Step()
{
    int res;
    if (cur_idx < 2)
        res = cur_idx == 0 ? n1 : n2;
    else {
        res = n1 + n2;
        n1 = n2;
        n2 = res;
    }

    cur_idx++;
    return res;
}

int main()
{
    int n;
    printf("Input N: ");
    if (scanf("%d", &n) != 1) {
        fprintf(stderr, "Input: <n>\n");
        return 1;
    }

    FibFSM fsm;
    for (int i = 0; i < n; i++)
        printf("%d ", fsm.Step());
    putchar('\n');

    return 0;
}
