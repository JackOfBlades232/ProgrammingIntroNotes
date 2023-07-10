/* parallel/readers_and_writers.c */
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

// The point is to allow readers to read at the same time, but 
// exclude everybody when writers are writing

// Readers will calc and display: sum, mean, prod, count, fullness ratio

#define WRITERS_COUNT 3
#define WRITER_SLEEP 1
#define READER_SLEEP 4
#define BUFSIZE 12

struct db_buffer {
    int write_idx;
    int content[BUFSIZE];
};

static struct db_buffer buf;
static int rc; // reader count

static sem_t db_access_sem;
static pthread_mutex_t rc_mutex = PTHREAD_MUTEX_INITIALIZER,
                       barrier_mutex = PTHREAD_MUTEX_INITIALIZER;
// Barrier mutex is for a writer to signal that he wants to write and
// block new readers from queueing up

typedef int (*statfunc)(struct db_buffer *);

struct reader_data {
    int idx;
    statfunc sf;
    char *name;
};

int sum_stat(struct db_buffer *buf);
int mean_stat(struct db_buffer *buf);
int prod_stat(struct db_buffer *buf);
int count_stat(struct db_buffer *buf);
int ratio_stat(struct db_buffer *buf);

static struct reader_data reader_datas[] = {
    { .idx = 0, .sf = sum_stat, .name = "sum" },
    { .idx = 1, .sf = mean_stat, .name = "mean" },
    { .idx = 2, .sf = prod_stat, .name = "prod" },
    { .idx = 3, .sf = count_stat, .name = "count" },
    { .idx = 4, .sf = ratio_stat, .name = "ratio" }
};

#define READERS_COUNT sizeof(reader_datas)/sizeof(*reader_datas)

void print_buf(struct db_buffer *buf)
{
    for (int i = 0; i < BUFSIZE; i++)
        printf(", %d", buf->content[i]);
    putchar('\n');
}

int sum_stat(struct db_buffer *buf)
{
    int sum = 0;
    for (int i = 0; i < BUFSIZE; i++) {
        if (buf->content[i] >= 0)
            sum += buf->content[i];
    }
    return sum;
}

int mean_stat(struct db_buffer *buf)
{
    return sum_stat(buf) / BUFSIZE;
}

int prod_stat(struct db_buffer *buf)
{
    int prod = 1;
    for (int i = 0; i < BUFSIZE; i++) {
        if (buf->content[i] >= 0)
            prod *= buf->content[i];
    }
    return prod;
}

int count_stat(struct db_buffer *buf)
{
    int cnt = 0;
    for (int i = 0; i < BUFSIZE; i++) {
        if (buf->content[i] >= 0)
            cnt++;
    }
    return cnt;
}

int ratio_stat(struct db_buffer *buf)
{
    return (int) (((float) count_stat(buf) / BUFSIZE) * 100);
}

void *writer_thread(void *data)
{
    for (;;) {
        pthread_mutex_lock(&barrier_mutex);
        sem_wait(&db_access_sem);

        buf.content[buf.write_idx] = time(NULL);        
        printf("Writer, put %d in place %d\n", buf.content[buf.write_idx], buf.write_idx);
        buf.write_idx = (buf.write_idx + 1) % BUFSIZE;

        sem_post(&db_access_sem);
        pthread_mutex_unlock(&barrier_mutex);

        sleep(WRITER_SLEEP);
    }
}

void *reader_thread(void *data)
{
    struct reader_data *rd = data;
    int idx = rd->idx;
    statfunc sf = rd->sf;
    char *stat_name = rd->name;

    for (;;) {
        pthread_mutex_lock(&barrier_mutex);
        pthread_mutex_unlock(&barrier_mutex);

        pthread_mutex_lock(&rc_mutex);
        rc++;
        if (rc == 1)
            sem_wait(&db_access_sem);
        pthread_mutex_unlock(&rc_mutex);

        printf("Reader %d; %s: %d, content", idx, stat_name, sf(&buf));
        print_buf(&buf);

        pthread_mutex_lock(&rc_mutex);
        rc--;
        if (rc == 0)
            sem_post(&db_access_sem);
        pthread_mutex_unlock(&rc_mutex);

        sleep(READER_SLEEP);
    }
}

int main(int argc, char **argv)
{
    int i;

    buf.write_idx = 0;
    for (i = 0; i < BUFSIZE; i++)
        buf.content[i] = -1;

    sem_init(&db_access_sem, 0, 1);

    pthread_t thr;
    for (i = 0; i < WRITERS_COUNT; i++)
        pthread_create(&thr, NULL, writer_thread, NULL);
    for (i = 0; i < READERS_COUNT; i++)
        pthread_create(&thr, NULL, reader_thread, &reader_datas[i]);

    for (;;) {}

    return 0;
}
