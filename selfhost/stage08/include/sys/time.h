/* sys/time.h -- s12cc-compatible stub
 *
 * Just struct itimerval + setitimer for dbt's probe timer (SIGALRM-based
 * sampling of the dispatch loop). Implementation in libc_a64/signal_stubs.c
 * is a no-op for now.
 */
#ifndef _SYS_TIME_H
#define _SYS_TIME_H

struct timeval {
    long tv_sec;
    long tv_usec;
};

struct itimerval {
    struct timeval it_interval;
    struct timeval it_value;
};

#define ITIMER_REAL    0
#define ITIMER_VIRTUAL 1
#define ITIMER_PROF    2

int setitimer(int which, struct itimerval *new, struct itimerval *old);
int getitimer(int which, struct itimerval *cur);
int gettimeofday(struct timeval *tv, void *tz);

#endif
