/* sys/resource.h — s12cc-compatible stub
 *
 * Just enough of POSIX resource control to let dbt-x64 raise its own
 * RLIMIT_STACK at startup.  cc-x64-built dbt-x64 needs roughly 20 MB
 * of host stack on the Forth/stage02 self-host workload (vs ~136 KB for
 * gcc-built slow32-dbt) due to less aggressive register allocation.
 *
 * Implementation in selfhost/stage07-cross/libc_x64/syscalls.c.
 */
#ifndef _SYS_RESOURCE_H
#define _SYS_RESOURCE_H

/* Resource codes — match the Linux ABI. */
#define RLIMIT_CPU     0
#define RLIMIT_FSIZE   1
#define RLIMIT_DATA    2
#define RLIMIT_STACK   3
#define RLIMIT_CORE    4
#define RLIMIT_RSS     5
#define RLIMIT_NPROC   6
#define RLIMIT_NOFILE  7
#define RLIMIT_MEMLOCK 8
#define RLIMIT_AS      9

/* Per the Linux x86-64 ABI, struct rlimit fields are 8-byte unsigned.
 * Modeled here as `unsigned long`; cc-x64 sets ty_ptr_size=8, so this
 * is 8 bytes, matching the kernel layout.  Hand-roll the fields rather
 * than typedef'ing rlim_t so the s12cc-subset doesn't need <stdint.h>. */
struct rlimit {
    unsigned long rlim_cur;
    unsigned long rlim_max;
};

#define RLIM_INFINITY ((unsigned long)-1)

int getrlimit(int resource, struct rlimit *rlim);
int setrlimit(int resource, struct rlimit *rlim);

#endif
