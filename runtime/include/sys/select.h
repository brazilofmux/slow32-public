#ifndef SLOW32_SYS_SELECT_H
#define SLOW32_SYS_SELECT_H

/* select, on poll (poll.c).  FD_SETSIZE is the machine's descriptor table,
 * 128.  The timeout is not updated with the time left (POSIX allows either;
 * Linux updates it). */

#include <sys/time.h>

#ifdef __cplusplus
extern "C" {
#endif

#define FD_SETSIZE 128
#define __NFDBITS  (8 * (int)sizeof(unsigned long))

typedef struct {
    unsigned long fds_bits[FD_SETSIZE / (8 * sizeof(unsigned long))];
} fd_set;

#define FD_ZERO(s)     __builtin_memset((s), 0, sizeof(fd_set))
#define FD_SET(fd, s)  ((void)((s)->fds_bits[(fd) / __NFDBITS] |= 1ul << ((fd) % __NFDBITS)))
#define FD_CLR(fd, s)  ((void)((s)->fds_bits[(fd) / __NFDBITS] &= ~(1ul << ((fd) % __NFDBITS))))
#define FD_ISSET(fd, s) (((s)->fds_bits[(fd) / __NFDBITS] >> ((fd) % __NFDBITS)) & 1ul)

int select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
           struct timeval *timeout);

#ifdef __cplusplus
}
#endif

#endif
