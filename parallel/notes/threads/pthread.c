/* threads/pthread.c */
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>

static int global_counter = 0;

// Mutex is some internal lib structure, to be inited with this macro
static pthread_mutex_t counter_mutex = PTHREAD_MUTEX_INITIALIZER;
// Semaphore is the same, but has to be inited in a function and
// can serve processes with shared mem, not only threads
static sem_t talking_stick_semaphore;

// Each thread has a main function, which takes anything as param
// and returns anything
void *thread_main(void *arg)
{
    pthread_mutex_lock(&counter_mutex); // Blocking version
    /* pthread_mutex_trylock(counter_mutex); */ // Nonblocking version
    global_counter++;
    pthread_mutex_unlock(&counter_mutex);

    sem_wait(&talking_stick_semaphore); // down
    /* sem_trywait(&talking_stick_semaphore); */ // Nonblocking version

    int sem_val;
    sem_getvalue(&talking_stick_semaphore, &sem_val);

    printf("Thread signing off, counter is %d, talking stick charges left %d\n",
            global_counter, sem_val);

    sem_post(&talking_stick_semaphore); // up

    pthread_exit(NULL);
    return NULL;
}

void *master_thread_main(void *arg)
{
    // detach makes the thread be freed completely on termination,
    // so as not to become a zombie
    pthread_detach(pthread_self());

    // can work with processes, so is not prefixed with pthread
    int sem_init_res = sem_init(&talking_stick_semaphore, 0, 4);
    // All semaphore funcs return 0/-1 and set errno
    if (sem_init_res == -1) {
        perror("sem_init");
        pthread_exit(NULL);
    }

    int num_threads = *((int *) arg);
    pthread_t *threads = malloc(num_threads * sizeof(*threads));
    int *thread_indicators = malloc(num_threads * sizeof(*thread_indicators));
    for (int i = 0; i < num_threads; i++) {
        // Takes pointer to thread, params, main thread func and void * input
        int cr_res = pthread_create(threads+i, NULL, thread_main, NULL);
        if (cr_res == EAGAIN) { // Only this code here
            perror("pthread_create");
            thread_indicators[i] = 0;
            continue;
        }

        thread_indicators[i] = 1;
    }

    for (int i = 0; i < num_threads; i++) {
        if (thread_indicators[i]) {
            // Can mark other thread for cancellation, will be cancelled
            // on cancellation points (like pthread_ calls)
            if (i % 2 != 0)
                pthread_cancel(threads[i]);

            // Analogous to waitpid, can also pass pointer to void * to collect res
            pthread_join(threads[i], NULL);
        }
    }

    // Destroys all data if mutex is open, otherwise fail
    // On linux mutex has no allocated data, but still this is good as a
    // scope check
    int md_res = pthread_mutex_destroy(&counter_mutex);
    if (md_res == EAGAIN)
        fprintf(stderr, "Mutex still in use!!\n");

    // Same with semaphores, but!! It does not check shit, just leads to
    // undef behaviour, so we must check ourselves
    sem_destroy(&talking_stick_semaphore);
    
    return NULL;
}

int main()
{
    int num_threads = 32;
    pthread_t master_thread;

    int res = pthread_create(&master_thread, NULL, master_thread_main, &num_threads);
    if (res == EAGAIN) { // Does not access errno, it seems
        perror("pthread_create");
        return 1;
    }

    for (;;) {}

    return 0;
}
