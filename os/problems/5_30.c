/* 5_26.c */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#define C

enum prog_type { redir_in, redir_out };

struct running_prog {
    const char *name;
    int argc;
    char **argv;
    int pid;
    enum prog_type type;
    int in_fd, out_fd;
};

static void init_running_prog_info(struct running_prog *rp, 
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

static int running_prog_info_is_blank(struct running_prog *rp)
{
    return rp->argc == 0;
}

static char **copy_argv_pointers(char **src_argv, int src_argc)
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

static void clean_pipe_end(struct running_prog *rp)
{
    if (rp->type == redir_in)
        close(rp->in_fd);
    else
        close(rp->out_fd);
}

static int run_prog(struct running_prog *rp)
{
    rp->argv = copy_argv_pointers(rp->argv, rp->argc);

    rp->pid = fork();
    if (rp->pid == 0) { /* child proc */
        redirect_io(rp);
        execvp(rp->name, rp->argv);
        perror(rp->name);
        _exit(1);
    }

    clean_pipe_end(rp);
    free(rp->argv);
    return rp->pid != -1;
}

void filter_io(int in_fd, int out_fd)
{
    FILE *in_f = fdopen(in_fd, "r");
    FILE *out_f = fdopen(out_fd, "w");
    int c;

    int print_next_char_code = 0;

    while ((c = getc(in_f)) != EOF) {
#if defined(A)
        if (print_next_char_code)
            putc(c, out_f);
        if (c == '\n')
            print_next_char_code = !print_next_char_code;
#elif defined(B)
        if (print_next_char_code == 1)
            putc(c, out_f);
        else if (print_next_char_code == 0)
            print_next_char_code = (c == ' ' || c == '\t') ? 1 : -1;
        if (c == '\n')
            print_next_char_code = 0;
#else
        if (print_next_char_code < 10 || c == '\n')
            putc(c, out_f);
        if (c == '\n')
            print_next_char_code = 0;
        else
            print_next_char_code++;
#endif
    }

    fflush(out_f);
}

int main(int argc, char **argv)
{
    struct running_prog rp1, rp2;
    struct running_prog *cur_progp;
    char **argp;

    int in_fd[2], out_fd[2];

    if (argc < 2) {
        fprintf(stderr, "Provide at least one prog to run\n");
        return 1;
    }

    pipe(in_fd);
    pipe(out_fd);

    init_running_prog_info(&rp1, redir_out, in_fd);
    init_running_prog_info(&rp2, redir_in, out_fd);
    
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

    filter_io(in_fd[0], out_fd[1]);

    close(in_fd[0]);
    close(out_fd[1]);

    while (wait(NULL) != -1) 
        {}

    return 0;
}
