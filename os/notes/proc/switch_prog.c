/* proc/switch_prog.c */
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>

int main(int argc, char **argv)
{
    char *cmdline[] = { "ls", "-l", "-a", "/var", NULL };

    fcntl(STDERR_FILENO, F_SETFD, FD_CLOEXEC); 
    /* by def exec progs have same fds, this changes it */

    /* syscall : execve -- substitute prog for proc (takes path, argv, env) */
    /* funcs for easier use: */
    switch (argc) {
        case 1: 
            execv("./fork", argv); /* inherits env */
            break;
        case 2: 
            execvp("ls", cmdline); /* if no slashes in name,
                                      search sis dirs in PATH */
            break;
        case 3: 
            execlp("ls", "ls", "-l", "-a", "/var", NULL); 
            /* same but with vaargs */
            break;
        case 4: 
            execl("./fork", "./fork", NULL);
            break;
        default:
            break;
    }

    /* if exec is successful, the prog switches, so best to always put 
     * perror->exit after exec */

    /* something like: */
    execlp("ls", "ls", "-l", NULL);
    perror("ls");
    fflush(stderr); /* to definitely print error */
    _exit(1); /* to avoid flushing io buffers (if parent also uses them) */

    return 0;
}
