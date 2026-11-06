#ifndef _SYS_TIMES_H
#define _SYS_TIMES_H

#include <time.h>

/* times() counts in these; sysconf(_SC_CLK_TCK) says the same. */
#define SLOW32_CLK_TCK 100

struct tms {
    clock_t tms_utime;
    clock_t tms_stime;
    clock_t tms_cutime;
    clock_t tms_cstime;
};

#ifdef __cplusplus
extern "C" {
#endif

clock_t times(struct tms *buf);

#ifdef __cplusplus
}
#endif

#endif
