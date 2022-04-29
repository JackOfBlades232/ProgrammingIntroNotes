#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>
#include <math.h>

#define BUFFER_SIZE 4096
#define CONSUMERS_COUNT 10
#define PAUSE_LENGTH 5

struct buf_str {
    int count;
    double values[BUFFER_SIZE];
} buffer;

void init_buffer()
{
    buffer.count = 0;
}
void put_buffer_item(double v)
{
    buffer.values[buffer.count] = v;
    buffer.count++;
}

double get_buffer_item()
{
    buffer.count--;
    return buffer.values[buffer.count];
}

sem_t buf_empty;
sem_t buf_full;
pthread_mutex_t buf_mutex = PTHREAD_MUTEX_INITIALIZER;

double grand_total = 0;
long grand_count = 0;
pthread_mutex_t grand_mutex = PTHREAD_MUTEX_INITIALIZER;

sem_t producers_count, data_pieces;

void *producer_thread(void *v)
{             /* v points to the name of the source */
    double val;
    FILE *f = fopen((char*)v, "r");
    if(!f) {
        perror((char*)v);
        sem_wait(&producers_count);
        return NULL;
    }
    while(!feof(f)) {
        if(1 != fscanf(f, "%lf", &val))
            continue;
        sem_post(&data_pieces);
        sem_wait(&buf_empty);     /* the Producer */
        pthread_mutex_lock(&buf_mutex);
        put_buffer_item(val);
        pthread_mutex_unlock(&buf_mutex);
        sem_post(&buf_full);      /*  -------     */
    }
    sem_wait(&producers_count);
    return NULL;
}

void *consumer_thread(void *ignored)
{
    for(;;) {
        double val;
        sem_wait(&buf_full);      /* The Consumer */
        pthread_mutex_lock(&buf_mutex);
        val = get_buffer_item();
        pthread_mutex_unlock(&buf_mutex);
        sem_post(&buf_empty);     /*   -------    */
        val = log(val);
        pthread_mutex_trylock(&grand_mutex);
        grand_total += val;
        grand_count++;
        pthread_mutex_unlock(&grand_mutex);
        sem_wait(&data_pieces);
    }
}

int main(int argc, char **argv)
{
    pthread_t thr;
    int i;
    init_buffer();
    sem_init(&buf_empty, 0, BUFFER_SIZE);
    sem_init(&buf_full, 0, 0);
    sem_init(&producers_count, 0, 0);
    sem_init(&data_pieces, 0, 0);
    for(i = 1; i < argc; i++) {
        sem_post(&producers_count);
        pthread_create(&thr, NULL, producer_thread, (void*)argv[i]);
    }
    for(i = 0; i < CONSUMERS_COUNT; i++)
        pthread_create(&thr, NULL, consumer_thread, NULL);
    usleep(100000);

    for(;;) {
        int p_c, d_p;
        double gt;
        long gc;
        pthread_mutex_lock(&grand_mutex);
        gt = grand_total;
        gc = grand_count;
        grand_total = 0;
        grand_count = 0;
        sem_getvalue(&producers_count, &p_c);
        sem_getvalue(&data_pieces, &d_p);
        pthread_mutex_unlock(&grand_mutex);
        if(gc > 0)
            printf("total average: %f (sum = %f; count = %ld)\n",
                   gt/((double)gc), gt, gc);
        else
            printf("No data yet...\n");
        printf("%d producers still active\n", p_c);
        printf("%d data pieces being processed\n", d_p);
        if(p_c == 0 && d_p == 0) {
            printf("No more producers, no more data\n");
            break;
        }
        sleep(PAUSE_LENGTH);
    }
    return 0;
}
