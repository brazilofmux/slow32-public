/* the shell's .timer: no process accounting on the guest */
#ifndef SLOW32_SYS_RESOURCE_H
#define SLOW32_SYS_RESOURCE_H
#include <sys/time.h>
#include <string.h>
#define RUSAGE_SELF 0
struct rusage { struct timeval ru_utime, ru_stime; };
static inline int getrusage(int who, struct rusage *r) { (void)who; memset(r, 0, sizeof *r); return 0; }
#endif
