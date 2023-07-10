/* parallel/philosophers.c */
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// Philosophers trying to eat with chopsticks

#define PHILOSOPHERS_CNT 5
#define THINK_TIME 1
#define EAT_TIME 0
#define PAUSE_LENGTH 4

enum philosopher_state { hungry, eating, thinking };
struct philosopher {
    enum philosopher_state state;
    sem_t can_eat_sem;
    struct philosopher *left, *right;
    int times_nourished;
};

static struct philosopher philosophers[PHILOSOPHERS_CNT];
static pthread_mutex_t states_mutex = PTHREAD_MUTEX_INITIALIZER;

void test(struct philosopher *self)
{
    if (self->state == hungry &&
        self->left->state != eating && self->right->state != eating)
    {
        self->state = eating;
        sem_post(&self->can_eat_sem);
    }
}

void take_forks(struct philosopher *self)
{
    pthread_mutex_lock(&states_mutex);
    self->state = hungry;
    test(self);
    pthread_mutex_unlock(&states_mutex);
    sem_wait(&self->can_eat_sem);
}

void put_forks(struct philosopher *self)
{
    pthread_mutex_lock(&states_mutex);
    self->state = thinking;
    test(self->left);   // inform neighbours that i am done
    test(self->right);
    pthread_mutex_unlock(&states_mutex);
}

void eat(struct philosopher *self)
{
    sleep(EAT_TIME);
    self->times_nourished++;
}

void *philosopher_thread(void *v_data)
{
    struct philosopher *self = v_data;

    for (;;) {
        sleep(THINK_TIME);
        take_forks(self);
        eat(self);
        put_forks(self);
    }
}

void init_philosopher(struct philosopher *ph, int idx)
{
    ph->state = thinking;
    sem_init(&ph->can_eat_sem, 0, 0);
    ph->left = philosophers + (idx - 1 + PHILOSOPHERS_CNT) % PHILOSOPHERS_CNT;
    ph->right = philosophers + (idx + 1) % PHILOSOPHERS_CNT;
    ph->times_nourished = 0;
}

void log_philosopher(struct philosopher *ph, int idx)
{
    printf("Philosopher %d: ", idx);
    printf(ph->state == thinking ? "thinking" : (ph->state == hungry ? "hungry" : "eating"));
    printf(", eaten %d times\n", ph->times_nourished);
}

int main(int argc, char **argv)
{
    int i;

    for (i = 0; i < PHILOSOPHERS_CNT; i++)
        init_philosopher(philosophers+i, i);

    pthread_t thr;
    for (i = 0; i < PHILOSOPHERS_CNT; i++)
        pthread_create(&thr, NULL, philosopher_thread, philosophers+i);

    for (;;) {
        pthread_mutex_lock(&states_mutex);

        for (i = 0; i < PHILOSOPHERS_CNT; i++)
            log_philosopher(philosophers+i, i);
        putchar('\n');

        pthread_mutex_unlock(&states_mutex);
        sleep(PAUSE_LENGTH);
    }

    return 0;
}
