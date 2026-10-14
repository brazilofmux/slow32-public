/* sys/select.h -- Linux select(2): a 1024-bit fd_set (cross builds). */
#ifndef _SYS_SELECT_H
#define _SYS_SELECT_H
#include <sys/time.h>
#define FD_SETSIZE 1024
typedef struct {
    unsigned long fds_bits[FD_SETSIZE / 64];
} fd_set;
#define FD_ZERO(s)    do { int __i; for (__i = 0; __i < FD_SETSIZE / 64; __i++) (s)->fds_bits[__i] = 0; } while (0)
#define FD_SET(d, s)  ((s)->fds_bits[(d) / 64] |= (1UL << ((d) % 64)))
#define FD_CLR(d, s)  ((s)->fds_bits[(d) / 64] &= ~(1UL << ((d) % 64)))
#define FD_ISSET(d, s) (((s)->fds_bits[(d) / 64] & (1UL << ((d) % 64))) != 0)
int select(int nfds, fd_set *r, fd_set *w, fd_set *e, struct timeval *timeout);
#endif
