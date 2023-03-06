/* proc/times.c */
#include <sys/time.h>
#include <sys/resource.h>
#include <time.h>
#include <unistd.h>

int main()
{
    time_t cur_t;
    struct tm cur_tm;
    char buf[128];
    struct timeval tv;
    struct timezone tz;
    struct timespec req, rem;
    int prioity;

    cur_t = time(NULL); /* get sec from epoch */
    gmtime_r(&cur_t, &cur_tm); /* get struct with time info */
    asctime_r(&cur_tm, buf); /* to-string funcs */
    ctime_r(&cur_t, buf);

    /* there are other different funcs. _r fill given mem, no _r return pointer
     * to some static mem, so other call overwrite it */
    gettimeofday(&tv, &tz); /* to get sec and usec from start of day */

    sleep(1); /* seconds proc sleep, retuns 0 if slept, else int for signal */
    usleep(1000000); /* same, usec, commonly takes <= 1000000 */

    req.tv_sec = 2;
    req.tv_nsec = 500000;
    nanosleep(&req, &rem); /* waits for frac, remaining time in rem */

    prioity = nice(0); /* inc pr value and ret new (+val means less priority)
                          proc can only make itself less important */
    nice(1); /* root-righted procs can call nice with neg vals */
    /* priority is inherited by children and stays the same with execve */
    /* priority ranges in -20..19 (rarely ..20) */

    prioity = getpriority(PRIO_PROCESS, getpid());
    setpriority(PRIO_PROCESS, getpid(), 19);
    /* manual setting/getting of prio for procs, same constr as nice */

    return 0;
}
