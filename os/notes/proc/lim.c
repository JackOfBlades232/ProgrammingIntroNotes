/* proc/lim.c */
#include <sys/time.h>
#include <sys/resource.h>

int main()
{
    int res;
    struct rlimit rlim;
    res = getrlimit(RLIMIT_STACK, &rlim); /* limit of a resource for proc */
    rlim.rlim_cur = rlim.rlim_max; /* cur can be changed however, while <=max
                                      and max can be decreased, or chaged for
                                      su procs. */
    setrlimit(RLIMIT_STACK, &rlim);

    /* limits are also inherited */

    getrlimit(RLIMIT_CPU, &rlim);
    rlim.rlim_cur = RLIM_INFINITY; /* for no cap, usually -1 */
    setrlimit(RLIMIT_CPU, &rlim);
    return 0;
}

