#ifndef S32_HOST_POLL_H
#define S32_HOST_POLL_H

/* Shared by tools/emulator/mmio_ring.c and QEMU's target/slow32/mmio.c,
 * which keeps a copy of this file. */

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdbool.h>
#include <sys/select.h>

/* poll(2), with macOS's gap filled (s32_host_poll).  Its poll() answers POLLNVAL for a
 * character device -- a tty, /dev/null -- that is open and readable, so a
 * terminal or a /dev/null stdin was never "ready" to POST_READ, POLL or
 * KEY_AVAIL on a Mac while it was at once on Linux (readiness is "a read
 * would not block": docs/SPEC.md 8.12).  For such an fd the answer comes
 * from select(2), and when one is in the set the wait is a select over all
 * of them; the other fds' bits then come from a second, immediate poll.
 * EINTR is retried. */
static inline int s32_host_poll(struct pollfd *pf, nfds_t n, int timeout) {
    int r;
#if defined(__APPLE__)
    bool dev[16] = { false }, anydev = false;
    if (n > 16) {
        while ((r = poll(pf, n, timeout)) == -1 && errno == EINTR) { }
        return r;
    }
    while ((r = poll(pf, n, 0)) == -1 && errno == EINTR) { }
    if (r < 0) return r;
    bool ready = false;
    for (nfds_t i = 0; i < n; i++) {
        if ((pf[i].revents & POLLNVAL) && pf[i].fd >= 0 && fcntl(pf[i].fd, F_GETFD) != -1) {
            dev[i] = anydev = true;
            pf[i].revents = 0;
        } else if (pf[i].revents) {
            ready = true;
        }
    }
    if (!anydev) {
        if (ready || timeout == 0) return r;
        while ((r = poll(pf, n, timeout)) == -1 && errno == EINTR) { }
        return r;
    }
    fd_set rf;
    FD_ZERO(&rf);
    int maxfd = -1;
    for (nfds_t i = 0; i < n; i++) {
        if (pf[i].fd < 0 || pf[i].fd >= FD_SETSIZE) continue;
        if (!dev[i] && (pf[i].revents & POLLNVAL)) continue;
        FD_SET(pf[i].fd, &rf);
        if (pf[i].fd > maxfd) maxfd = pf[i].fd;
    }
    struct timeval tv = { 0, 0 }, *tvp = &tv;
    if (!ready && timeout != 0) {
        if (timeout < 0) tvp = NULL;
        else { tv.tv_sec = timeout / 1000; tv.tv_usec = (timeout % 1000) * 1000; }
    }
    while ((r = select(maxfd + 1, &rf, NULL, NULL, tvp)) == -1 && errno == EINTR) { }
    if (r < 0) return r;
    int count = 0;
    for (nfds_t i = 0; i < n; i++) {
        if (dev[i]) {
            pf[i].revents = FD_ISSET(pf[i].fd, &rf) ? POLLIN : 0;
        } else if (pf[i].fd >= 0) {
            struct pollfd one = { .fd = pf[i].fd, .events = pf[i].events };
            while (poll(&one, 1, 0) == -1 && errno == EINTR) { }
            pf[i].revents = one.revents;
        }
        if (pf[i].revents) count++;
    }
    return count;
#else
    while ((r = poll(pf, n, timeout)) == -1 && errno == EINTR) { }
    return r;
#endif
}

#endif
