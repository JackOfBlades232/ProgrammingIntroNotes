/* 5_13.c */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

struct running_prog {
    const char *name;
    int argc;
    char **argv;
    int pid;
};

void init_running_prog_info(struct running_prog *rp)
{
    rp->name = NULL;
    rp->argc = 0;
    rp->argv = NULL;
    rp->pid = -1;
}

int running_prog_info_is_blank(struct running_prog *rp)
{
    return rp->argc == 0;
}

char **copy_argv_pointers(char **src_argv, int src_argc)
{
    char **dest_argv = malloc((src_argc+1) * sizeof(char *));
    memcpy(dest_argv, src_argv, src_argc * sizeof(char *));
    dest_argv[src_argc] = NULL;
    return dest_argv;
}

struct running_prog *run_prog(const char *name, int argc, char **argv)
{
    struct running_prog *rp = malloc(sizeof(struct running_prog));
    rp->name = name;
    rp->argc = argc;
    rp->argv = copy_argv_pointers(argv, argc);

    rp->pid = fork();
    if (rp->pid == -1) { /* error in fork */
        free(rp->argv);
        free(rp);
        return NULL;
    } else if (rp->pid == 0) { /* child proc */
        execvp(rp->name, rp->argv);
        perror(rp->name);
        _exit(1);
    }

    return rp;
}

void free_running_prog_arr(struct running_prog **progs)
{
    struct running_prog **rpp;
    for (rpp = progs; *rpp; rpp++) {
        free((*rpp)->argv);
        free(*rpp);
    }
    free(progs);
}

const char *prog_name_by_pid(struct running_prog **progs, int pid)
{
    struct running_prog **rpp;

    for (rpp = progs; *rpp; rpp++) {
        if ((*rpp)->pid == pid)
            return (*rpp)->name;
    }

    return NULL;
}

int main(int argc, char **argv)
{
    struct running_prog **progs, **rpp;
    struct running_prog next_prog_info;
    char **argp;

    int wr, status;

    if (argc < 2) {
        fprintf(stderr, "Provide at least one prog to run\n");
        return 1;
    }

    progs = malloc((argc / 2) * sizeof(struct running_prog *));
    rpp = progs;
    init_running_prog_info(&next_prog_info);

    for (argp = argv+1; argp-argv <= argc; argp++) {
        int is_break = argp-argv == argc || strcmp(*argp, ";;") == 0;
        int is_blank = running_prog_info_is_blank(&next_prog_info);

        if (is_break && !is_blank) {
            *rpp = run_prog(
                    next_prog_info.name, 
                    next_prog_info.argc, 
                    next_prog_info.argv
                    );
            rpp++;
            init_running_prog_info(&next_prog_info);
        } 

        if (is_break)
            continue;

        if (is_blank) {
            next_prog_info.name = *argp;
            next_prog_info.argv = argp;
        }
            
        next_prog_info.argc++;
    }

    *rpp = NULL;

    while ((wr = wait(&status)) != -1) {
        if (WIFEXITED(status) && WEXITSTATUS(status) == 0)
            printf("%s\n", prog_name_by_pid(progs, wr));
    }

    free_running_prog_arr(progs);
    return 0;
}
