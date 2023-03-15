/* daemon.c */
#include <sys/syslog.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <syslog.h>

int main()
{
    /* a daemon shall not have std descriptors, a controlling terminal and
     * should not be a session leader, so this is a standard daemon prep */
    int pid;

    close(0); /* close standard descriptors and replace them with /dev/null */
    close(1);
    close(2);
    open("/dev/null", O_RDONLY); /* stdin */
    open("/dev/null", O_WRONLY); /* stdout */
    open("/dev/null", O_WRONLY); /* stderr */

    /* change root dir to / to not be affected by any changes */
    chdir("/");

    /* checkout to new session */
    pid = fork();
    if (pid > 0)
        exit(0);
    setsid();

    /* fork out again not to be the session leader */
    pid = fork();
    if (pid > 0)
        exit(0);

    /* now we can work with system logging */
    openlog("test_daemon", 0, LOG_USER);
    syslog(LOG_INFO, "Daemon started, pid == %d", getpid());
    closelog();

    /* one usually controls daemons with signals */

    return 0;
}
