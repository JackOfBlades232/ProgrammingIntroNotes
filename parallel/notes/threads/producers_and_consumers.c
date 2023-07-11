/* threads/producers_and_consumers.c */
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

// Producers capitalize letters, consumers put back down

#define PRODUCERS_COUNT 3
#define CONSUMERS_COUNT 5
#define PAUSE_LENGTH 1

struct word_buffer {
    int size, capped;
    char **words;
};

static struct word_buffer buf;

static sem_t empty_sem, full_sem;
static pthread_mutex_t buf_mutex = PTHREAD_MUTEX_INITIALIZER;

int word_is_alphabetic(const char *word)
{
    for (; *word; word++) {
        if ((*word < 'a' || *word > 'z') &&
            (*word < 'A' || *word > 'Z')) {
            return 0;
        }
    }

    return 1;
}

int word_is_uncapped(const char *word)
{
    for (; *word; word++) {
        if (*word >= 'a' && *word <= 'z')
            return 1;
    }

    return 0;
}

int word_is_capped(const char *word)
{
    return !word_is_uncapped(word);
}

char *find_word(const struct word_buffer *buf, int (*cond)(const char *))
{
    for (int i = 0; i < buf->size; i++) {
        if (cond(buf->words[i]))
            return buf->words[i];
    }

    return NULL;
}

void upcase_word(char *word)
{
    for (; *word; word++) {
        if (*word >= 'a' && *word <= 'z')
            *word += 'A' - 'a';
    }
}

void downcase_word(char *word)
{
    for (; *word; word++) {
        if (*word >= 'A' && *word <= 'Z')
            *word += 'a' - 'A';
    }
}

void *producer_thread(void *data)
{
    for (;;) {
        sem_wait(&empty_sem);
        pthread_mutex_lock(&buf_mutex);

        char *word = find_word(&buf, word_is_uncapped);
        if (!word) {
            fprintf(stderr, "ERR: No words to be capped, semaphore bug\n");
            exit(-1);
        }
        upcase_word(word);
        buf.capped++;

        pthread_mutex_unlock(&buf_mutex);
        sem_post(&full_sem);
    }
}

void *consumer_thread(void *data)
{
    for (;;) {
        sem_wait(&full_sem);
        pthread_mutex_lock(&buf_mutex);

        char *word = find_word(&buf, word_is_capped);
        if (!word) {
            fprintf(stderr, "ERR: No words to be downcast, semaphore bug\n");
            exit(-1);
        }
        downcase_word(word);
        buf.capped--;

        pthread_mutex_unlock(&buf_mutex);
        sem_post(&empty_sem);
    }
}

int main(int argc, char **argv)
{
    int i;

    buf.size = argc-1;
    buf.capped = 0;
    buf.words = argv+1;

    if (buf.size < 1) {
        fprintf(stderr, "Args: at least one arr word is required\n");
        return -1;
    }

    for (i = 0; i < buf.size; i++) {
        if (!word_is_alphabetic(buf.words[i])) {
            fprintf(stderr, "Args: words must only include letters\n");
            return -1;
        }

        downcase_word(buf.words[i]);
    }

    sem_init(&empty_sem, 0, 0);
    sem_init(&full_sem, 0, 0);
    for (i = 0; i < buf.size; i++)
        sem_post(&empty_sem);

    pthread_t thr;
    for (i = 0; i < PRODUCERS_COUNT; i++)
        pthread_create(&thr, NULL, producer_thread, NULL);
    for (i = 0; i < CONSUMERS_COUNT; i++)
        pthread_create(&thr, NULL, consumer_thread, NULL);

    for (;;) {
        pthread_mutex_lock(&buf_mutex);

        printf("%d capped, %d not capped: ", buf.capped, buf.size-buf.capped);
        for (i = 0; i < buf.size; i++)
            printf("%s ", buf.words[i]);
        printf("\n");

        pthread_mutex_unlock(&buf_mutex);
        sleep(PAUSE_LENGTH);
    }

    return 0;
}
