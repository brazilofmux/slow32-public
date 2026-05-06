/* time.h -- s12cc-compatible stub */
#ifndef _TIME_H
#define _TIME_H

typedef long time_t;

struct timespec {
    long tv_sec;
    long tv_nsec;
};

#define CLOCK_REALTIME  0
#define CLOCK_MONOTONIC 1
typedef int clockid_t;

int clock_gettime(clockid_t clk, struct timespec *ts);
time_t time(time_t *t);

#endif
