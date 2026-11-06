#ifndef SHIM_TIMES_H
#define SHIM_TIMES_H
#include <time.h>
struct tms { clock_t tms_utime, tms_stime, tms_cutime, tms_cstime; };
#ifdef __cplusplus
extern "C"
#endif
clock_t times(struct tms *);
#endif
