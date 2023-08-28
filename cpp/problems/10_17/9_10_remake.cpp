/* 9_10.c */
#include <stdio.h>

class IntQueue {
    struct intq_item {
        int val;
        struct intq_item *next;
    };
    intq_item *head, *tail;

public:
    IntQueue() : head(0), tail(0) {}
    ~IntQueue();
    int Head() const { return head ? head->val : -1; }
    void Enqueue(int val);
    int Dequeue();
};

struct pas_value {
    int row, idx_in_row, val;
};

class PasFSM {
    int row, idx_in_row;
    IntQueue cache;

public:
    PasFSM() : row(0), idx_in_row(0), cache() {}
    ~PasFSM() {}
    pas_value Step();
};

IntQueue::~IntQueue()
{
    while (head)
        Dequeue();
}

void IntQueue::Enqueue(int val)
{
    intq_item *elem = new intq_item;
    elem->val = val;
    elem->next = NULL;

    if (tail)
        tail->next = elem;
    else
        head = elem;
    tail = elem;
}

int IntQueue::Dequeue()
{
    if (!head)
        return -1;

    intq_item *tmp = head;
    head = tmp->next;
    if (!head)
        tail = NULL;

    int val = tmp->val;
    delete tmp;
    return val;
}

pas_value PasFSM::Step()
{
    pas_value res;
    res.row = row;
    res.idx_in_row = idx_in_row;

    if (idx_in_row == 0)
        res.val = 1; 
    else if (idx_in_row == row) {
        res.val = 1;
        cache.Dequeue();
    } else {
        int n1 = cache.Dequeue();
        int n2 = cache.Head();
        res.val = n1 + n2;
    }

    if (idx_in_row == row) {
        row++;
        idx_in_row = 0;
    } else
        idx_in_row++;

    cache.Enqueue(res.val);
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

    PasFSM fsm;

    pas_value elem;
    for (int i = 0; i < n; i++) {
        elem = fsm.Step();
        printf("(%d, %d, %d) ", elem.row, elem.idx_in_row, elem.val);
    }
    putchar('\n');

    return 0;
}
