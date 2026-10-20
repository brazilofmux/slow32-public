/* poll and select, for the machine's descriptors.
 *
 * poll is one request (FDPOLL, docs/SPEC.md): the pollfd array goes to the
 * host as it is -- its layout is the wire format -- the host waits on the
 * descriptors behind it, and the revents come back.  select is poll with
 * the sets turned into an array and back, with Linux's reading of which
 * revents make a descriptor "ready" for each set. */
#include <errno.h>
#include <limits.h>
#include <poll.h>
#include <string.h>
#include <sys/select.h>

#include "mmio_ring.h"

_Static_assert(sizeof(struct pollfd) == sizeof(s32_mmio_pollfd_t),
               "struct pollfd is the FDPOLL wire format");

int poll(struct pollfd *fds, nfds_t nfds, int timeout) {
    if (nfds > S32_MMIO_FDPOLL_MAX) {
        errno = EINVAL;
        return -1;
    }
    unsigned len = (unsigned)nfds * sizeof(struct pollfd);
    if (len) memcpy((void *)S32_MMIO_DATA_BUFFER, fds, len);
    unsigned r = (unsigned)s32_mmio_request(S32_MMIO_OP_FDPOLL, len, 0u, (unsigned)timeout);
    if (r == S32_MMIO_STATUS_ERR) return -1;
    const struct pollfd *back = (const struct pollfd *)S32_MMIO_DATA_BUFFER;
    for (nfds_t i = 0; i < nfds; i++) fds[i].revents = back[i].revents;
    return (int)r;
}

#define READ_READY  (POLLIN | POLLRDNORM | POLLRDBAND | POLLHUP | POLLERR)
#define WRITE_READY (POLLOUT | POLLWRNORM | POLLWRBAND | POLLERR)
#define EXCEPT_READY POLLPRI

int select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
           struct timeval *timeout) {
    struct pollfd pf[FD_SETSIZE];
    int n = 0, ms = -1;

    if (nfds < 0 || nfds > FD_SETSIZE) {
        errno = EINVAL;
        return -1;
    }
    if (timeout) {
        if (timeout->tv_sec < 0 || timeout->tv_usec < 0 || timeout->tv_usec >= 1000000) {
            errno = EINVAL;
            return -1;
        }
        long long t = (long long)timeout->tv_sec * 1000 + (timeout->tv_usec + 999) / 1000;
        ms = t > INT_MAX ? INT_MAX : (int)t;
    }
    for (int fd = 0; fd < nfds; fd++) {
        short ev = 0;
        if (readfds && FD_ISSET(fd, readfds)) ev |= POLLIN;
        if (writefds && FD_ISSET(fd, writefds)) ev |= POLLOUT;
        if (exceptfds && FD_ISSET(fd, exceptfds)) ev |= POLLPRI;
        if (ev) {
            pf[n].fd = fd;
            pf[n].events = ev;
            pf[n].revents = 0;
            n++;
        }
    }
    if (poll(pf, (nfds_t)n, ms) < 0) return -1;
    for (int i = 0; i < n; i++) {
        if (pf[i].revents & POLLNVAL) {
            errno = EBADF;
            return -1;
        }
    }

    int count = 0;
    for (int i = 0; i < n; i++) {
        int fd = pf[i].fd;
        short rev = pf[i].revents;
        if (readfds && FD_ISSET(fd, readfds)) {
            if (rev & READ_READY) count++;
            else FD_CLR(fd, readfds);
        }
        if (writefds && FD_ISSET(fd, writefds)) {
            if (rev & WRITE_READY) count++;
            else FD_CLR(fd, writefds);
        }
        if (exceptfds && FD_ISSET(fd, exceptfds)) {
            if (rev & EXCEPT_READY) count++;
            else FD_CLR(fd, exceptfds);
        }
    }
    return count;
}
