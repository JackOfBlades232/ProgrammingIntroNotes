/* 5_13.c */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

enum prog_type { redir_in, redir_out };

struct running_prog {
    const char *name;
    int argc;
    char **argv;
    int pid;
    enum prog_type type;
    int in_fd, out_fd;
};

void init_running_prog_info(struct running_prog *rp, 
        enum prog_type type, int fd[2])
{
    rp->name = NULL;
    rp->argc = 0;
    rp->argv = NULL;
    rp->pid = -1;

    rp->type = type;
    rp->in_fd = fd[0];
    rp->out_fd = fd[1];
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

static void redirect_io(struct running_prog *rp)
{
    if (rp->type == redir_in)
        dup2(rp->in_fd, STDIN_FILENO);
    else
        dup2(rp->out_fd, STDOUT_FILENO);
    
    close(rp->in_fd);
    close(rp->out_fd);
}

int run_prog(struct running_prog *rp)
{
    rp->argv = copy_argv_pointers(rp->argv, rp->argc);

    rp->pid = fork();
    if (rp->pid == 0) { /* child proc */
        redirect_io(rp);
        execvp(rp->name, rp->argv);
        perror(rp->name);
        _exit(1);
    }

    free(rp->argv);
    return rp->pid != -1;
}

int main(int argc, char **argv)
{
    struct running_prog rp1, rp2;
    struct running_prog *cur_progp;
    char **argp;

    int fd[2];

    if (argc < 2) {
        fprintf(stderr, "Provide at least one prog to run\n");
        return 1;
    }

    pipe(fd);

    init_running_prog_info(&rp1, redir_out, fd);
    init_running_prog_info(&rp2, redir_in, fd);
    
    cur_progp = &rp1;

    for (argp = argv+1; argp-argv <= argc; argp++) {
        int is_break = argp-argv == argc || strcmp(*argp, ";;") == 0;
        int is_blank = running_prog_info_is_blank(cur_progp);

        if (is_break && !is_blank) {
            int success = run_prog(cur_progp);
            if (!success) {
                fprintf(stderr, "Failed to launch proc\n");
                return 2;
            }

            if (cur_progp == &rp1)
                cur_progp = &rp2;
            else if (argp-argv < argc) {
                fprintf(stderr, "Provide no more than 2 progs\n");
                return 3;
            }
        } 

        if (is_break)
            continue;

        if (is_blank) {
            cur_progp->name = *argp;
            cur_progp->argv = argp;
        }
            
        cur_progp->argc++;
    }

    close(fd[0]);
    close(fd[1]);

    while (wait(NULL) != -1) 
        {}

    return 0;
}
