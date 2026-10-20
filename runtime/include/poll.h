#ifndef SLOW32_POLL_H
#define SLOW32_POLL_H

/* Readiness of descriptors (poll.c): the host waits on them, up to 128 --
 * the machine's whole descriptor table -- for as long as the timeout says
 * (milliseconds; negative, without end).  A negative fd is skipped; one
 * that is not open is POLLNVAL.  Linux's numbers. */

#ifdef __cplusplus
extern "C" {
#endif

#define POLLIN     0x001
#define POLLPRI    0x002
#define POLLOUT    0x004
#define POLLERR    0x008
#define POLLHUP    0x010
#define POLLNVAL   0x020
#define POLLRDNORM 0x040
#define POLLRDBAND 0x080
#define POLLWRNORM 0x100
#define POLLWRBAND 0x200

typedef unsigned long nfds_t;

struct pollfd {
    int fd;
    short events;
    short revents;
};

int poll(struct pollfd *fds, nfds_t nfds, int timeout);

#ifdef __cplusplus
}
#endif

#endif
