#ifndef SLOW32_TIME_H
#define SLOW32_TIME_H

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned int time_t;

struct timespec {
    time_t tv_sec;
    unsigned int tv_nsec;
};

#define CLOCK_REALTIME 0

int clock_gettime(int clock_id, struct timespec *ts);
time_t time(time_t *t);

#ifdef __cplusplus
}
#endif

#endif
