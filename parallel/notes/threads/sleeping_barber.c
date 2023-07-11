/* threads/sleeping_barber.c */
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// BArber trying to work and sleep

#define TOTAL_SEATS_COUNT 12
#define TOTAL_CLIENTS 50
#define CLIENT_DELAY 1

static int seats = TOTAL_SEATS_COUNT;
static int clients_served = 0;

static pthread_mutex_t seats_mutex = PTHREAD_MUTEX_INITIALIZER;
static sem_t waitlist_sem;
static sem_t barber_sem, client_sem, seatbelt_sem;

static sem_t client_idx_sem;

void *client_thread(void *data)
{
    int idx = *((int *) data);
    sem_post(&client_idx_sem);

    sleep(CLIENT_DELAY);

    pthread_mutex_lock(&seats_mutex);
    if (seats > 0) {
        seats--;
        sem_post(&waitlist_sem); // Wakes up barber
        pthread_mutex_unlock(&seats_mutex); // Sat down

        sem_wait(&barber_sem); // Wait for barber to be ready
        printf("Client %d is sitting down\n", idx);
        sem_post(&client_sem); // Allow barber to start cutting
        sem_wait(&seatbelt_sem); // Wait for end of haircut
    } else
        pthread_mutex_unlock(&seats_mutex); // Just walk away

    return NULL;
}

int main()
{
    sem_init(&waitlist_sem, 0, 0);
    sem_init(&barber_sem, 0, 0);
    sem_init(&client_sem, 0, 0);
    sem_init(&seatbelt_sem, 0, 0);

    sem_init(&client_idx_sem, 0, 0);

    pthread_t thr;
    for (int i = 0; i < TOTAL_CLIENTS; i++) {
        pthread_create(&thr, NULL, client_thread, &i);
        sem_wait(&client_idx_sem);
    }

    // Barber is main thread
    while (clients_served < TOTAL_CLIENTS) {
        sem_wait(&waitlist_sem); // Sleep until there is someone to cut

        pthread_mutex_lock(&seats_mutex);
        sem_post(&barber_sem); // Invite client
        seats++;
        pthread_mutex_unlock(&seats_mutex);

        sem_wait(&client_sem); // Wait for client to sit down
        clients_served++;
        printf("Served client, %d left\n", TOTAL_CLIENTS-clients_served);
        sem_post(&seatbelt_sem); // Release the client
    }

    return 0;
}
