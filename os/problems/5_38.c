/* 5_38.c */
#include <sys/syslog.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>

enum { log_freq = 300 }; /* seconds */

static const char log_file_name[] = "/tmp/5_38.log";
static const char syslog_ident[] = "5_38";

static const char log_format_string[] = 
    "Name: %s, pid: %d, seconds since start: %ld, SUGUSR1 cnt: %d \n";

static FILE *log_f;
static char *prog_name;
static time_t startup_time;

volatile static sig_atomic_t usr1_cnt = 0;

void write_log()
{
    int pid = getpid();
    time_t elapsed_time = time(NULL) - startup_time;
    syslog(LOG_INFO, log_format_string, prog_name, pid, elapsed_time, usr1_cnt);
    fprintf(log_f, log_format_string, prog_name, pid, elapsed_time, usr1_cnt);
    fflush(log_f);
}

void handler(int s)
{
    int save_errno = errno;
    signal(s, handler);
    if (s == SIGUSR1)
        usr1_cnt++;
    write_log();
    alarm(log_freq);
    errno = save_errno;
}

void daemonize_self()
{
    int pid;
    close(STDIN_FILENO);
    close(STDOUT_FILENO);
    close(STDERR_FILENO);
    open("/dev/null", O_RDONLY);
    open("/dev/null", O_WRONLY);
    open("/dev/null", O_WRONLY);
    chdir("/");
    pid = fork();
    if (pid > 0)
        exit(0);
    setsid();
    pid = fork();
    if (pid > 0)
        exit(0);
}

int main(int argc, char **argv)
{
    daemonize_self();

    openlog(syslog_ident, 0, LOG_USER);
    log_f = fopen(log_file_name, "w");
    if (!log_f)
        return 1;
    prog_name = argv[0];
    startup_time = time(NULL);

    signal(SIGUSR1, handler);
    signal(SIGALRM, handler);

    alarm(log_freq);
    for (;;)
        pause();

    return 0;
}
