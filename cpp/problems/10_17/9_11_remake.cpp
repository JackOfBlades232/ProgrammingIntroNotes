/* 9_10.c */
#include <stdio.h>
#include <string.h>

void swap_int(int &a, int &b)
{
    int tmp = a;
    a = b;
    b = tmp;
}

int factorial(int n)
{
    int res = 1;
    for (int i = 2; i <= n; i++) {
        res *= i;
        if (res < 0)
            return -1;
    }
    return res;
}

class OutbufLenException {
    int req_len, given_len;
    const char *msg;

public:
    OutbufLenException(int a_req_len, int a_given_len, const char *a_msg)
        : req_len(a_req_len), given_len(a_given_len), msg(a_msg) {}
    int GetReqLen() const { return req_len; }
    int GetGivenLen() const { return given_len; }
    const char *GetMsg() const { return msg; }
};

class PermFSM {
    int n;
    int *last_perm;
    int longest_inv_tail;
    bool done;

public:
    PermFSM(int a_n);
    ~PermFSM() { delete[] last_perm; }
    void Step(int *out, int out_len); 
};

PermFSM::PermFSM(int a_n)
    : n(a_n)
{
    last_perm = new int[n];
    for (int i = 0; i < n; i++)
        last_perm[i] = i+1;
    longest_inv_tail = 1;
    done = false;
}

void PermFSM::Step(int *out, int out_len) // TODO: add outbuf size
{
    if (out_len < n) {
        throw OutbufLenException(n, out_len, 
                "Outbuf len must be at least equal to permutation's n!");
    }

    memcpy(out, last_perm, n * sizeof(*last_perm));

    /* If already generated all permutations, just clear the outbuf */
    if (done)
        return;
    else if (!done && longest_inv_tail >= n) {
        memset(last_perm, 0, n * sizeof(*last_perm));
        done = true;
        return;
    }

    /* find the first element larger than pivot in tail
     * (the tail is the longest motonously decreasing suffix) */
    int pivot_idx = n - longest_inv_tail - 1;
    int next_idx;
    for (next_idx = n - 1; next_idx > pivot_idx; next_idx--) {
        if (last_perm[next_idx] > last_perm[pivot_idx])
            break;
    }

    /* swap it with the pivot */
    swap_int(last_perm[pivot_idx], last_perm[next_idx]);

    /* reverse the tail */
    for (int i = 0; i < longest_inv_tail / 2; i++)
        swap_int(last_perm[pivot_idx+i+1], last_perm[n-i-1]);

    /* reset the tail len: if tail was longer that 1 elem, after the reverse
     * it becomes 1 elem long, otherwise we have to recalculate it's len */
    if (longest_inv_tail > 1)
        longest_inv_tail = 1;
    else {
        for (int i = n-1; i >= 1; i--) {
            if (last_perm[i-1] <= last_perm[i])
                break;
            longest_inv_tail++;
        }
    }
}

int main()
{
    int n;
    printf("Input N: ");
    if (scanf("%d", &n) != 1 || n <= 0) {
        fprintf(stderr, "Input: <n>, n >= 1\n");
        return 1;
    }

    int nfac = factorial(n);
    if (nfac == -1) {
        fprintf(stderr, "N is too large!\n");
        return 1;
    }

    PermFSM fsm(n);
    int *outbuf = new int[n];

    for (int i = 0; i < nfac; i++) {
        try {
            fsm.Step(outbuf, n);
        } catch (const OutbufLenException &ex) {
            fprintf(stderr, "%s (%d/%d)\n", 
                    ex.GetMsg(), ex.GetGivenLen(), ex.GetReqLen());
            return 1;
        }

        putchar('{');
        for (int j = 0; j < n-1; j++)
            printf("%d, ", outbuf[j]);
        printf("%d}\n", outbuf[n-1]);
    }

    return 0;
}
