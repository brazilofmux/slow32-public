#ifndef SLOW32_SYS_RESOURCE_H
#define SLOW32_SYS_RESOURCE_H

/* Resource limits and usage (process.c).  The limits are what the machine
 * imposes: descriptors are the host's table of 128 (docs/SPEC.md), and
 * there are no core files; the machine enforces no other, and those are
 * RLIM_INFINITY.  A limit can be "set" only to what it already is.
 * getrusage reports the processor time clock() does, as user time; the
 * other fields are zero.  Linux's numbers. */

#include <sys/time.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned long long rlim_t;
#define RLIM_INFINITY (~(rlim_t)0)

struct rlimit {
    rlim_t rlim_cur;
    rlim_t rlim_max;
};

#define RLIMIT_CPU    0
#define RLIMIT_FSIZE  1
#define RLIMIT_DATA   2
#define RLIMIT_STACK  3
#define RLIMIT_CORE   4
#define RLIMIT_NOFILE 7
#define RLIMIT_AS     9

int getrlimit(int resource, struct rlimit *rlim);
int setrlimit(int resource, const struct rlimit *rlim);

struct rusage {
    struct timeval ru_utime;
    struct timeval ru_stime;
    long ru_maxrss;
    long ru_ixrss;
    long ru_idrss;
    long ru_isrss;
    long ru_minflt;
    long ru_majflt;
    long ru_nswap;
    long ru_inblock;
    long ru_oublock;
    long ru_msgsnd;
    long ru_msgrcv;
    long ru_nsignals;
    long ru_nvcsw;
    long ru_nivcsw;
};

#define RUSAGE_SELF      0
#define RUSAGE_CHILDREN  (-1)

int getrusage(int who, struct rusage *usage);

#ifdef __cplusplus
}
#endif

#endif
