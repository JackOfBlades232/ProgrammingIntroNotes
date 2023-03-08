/* 5_25.c */
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>

const char toms_song[] = 
    "Hey dol! merry dol! ring a dong dillo!\n"
    "Ring a dong! hop along! Fal lal the willow!\n"
    "Tom Bom, jolly Tom, Tom Bombadillo!\n";

void run_child_proc(int fd[2])
{
    int wr_res;
    close(fd[0]);
    wr_res = write(fd[1], toms_song, sizeof(toms_song)-1);
    _exit(wr_res == sizeof(toms_song)-1 ? 0 : 1);
}

int main()
{
    int fd[2];
    int pid;

    char buf[128];
    int read_res;

    pipe(fd);

    pid = fork();
    if (pid == -1) {
        fprintf(stderr, "Failed to spawn child proc\n");
        return 1;
    } else if (pid == 0)
        run_child_proc(fd);

    close(fd[1]);
    while ((read_res = read(fd[0], buf, sizeof(buf)-1)) > 0) {
        buf[read_res] = '\0';
        printf("%s", buf);
    }

    wait(NULL);
    close(fd[0]);

    if (read_res == -1) {
        fprintf(stderr, "Error reading from pipe\n");
        return 2;
    }

    return 0;
}
