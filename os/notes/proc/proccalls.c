/* proc/proccalls.c */
#include <stddef.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <grp.h>

extern char **environ; /* env variables for the proc, NULL-term */

int main()
{
    char *env_val;
    void *heap_adr;
    int n_groups;
    gid_t *groups;

    /* also "set" */
    printf("uid: %d, gid: %d, euid: %d, egid: %d, "
           "pid: %d, ppid: %d, sid: %d, pgid: %d\n",
           getuid(), getgid(), geteuid(), getegid(),
           getpid(), getppid(), getsid(getpid()), getpgid(getpid()));

    /* to mind: setuid and setgid also work with e(u/g)id, unless euid == 0,
     * then they set all 3 (real, effective, saved) */

    env_val = getenv("HOME"); /* work with environ through funcs */
    if (env_val) {
        unsetenv("HOME");
        setenv("HOME", env_val, 1);
    }

    chdir("../");

    /* set the root catalogue (will be thought of as /) */
    chroot("/home/hhf232/Projects/ProgrammingIntroNotes/");
    chdir("/os/notes/proc/");

    /* brk for asking for new mem */
    heap_adr = sbrk(0);
    heap_adr += 4 * getpagesize();
    brk(heap_adr);

    /* get process groups */
    n_groups = getgroups(0, NULL); /* just get num groups */
    groups = malloc(n_groups * sizeof(gid_t));
    getgroups(n_groups, groups);

    setgroups(n_groups, groups); /* will not work for non-root */

    return 0;
}
