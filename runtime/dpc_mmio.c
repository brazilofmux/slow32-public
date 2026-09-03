/* Timers and the DPC ring, guest side (docs/plans/dpc.md).  The host is
 * asked through the request ring like everything else; the DPC ring is
 * read directly, and only its tail is written. */
#include <stdint.h>
#include <string.h>
#include "errno.h"
#include "mmio_ring.h"
#include "s32dpc.h"

int s32_timer_start(unsigned int seconds, unsigned int nanoseconds, unsigned int cookie) {
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    s32_mmio_timepair64_t interval = { seconds, 0u, nanoseconds, 0u };
    memcpy((void *)data_buffer, &interval, sizeof(interval));
    unsigned int status = (unsigned int)s32_mmio_request(S32_MMIO_OP_TIMER_START,
                                                         sizeof(interval), 0u, cookie);
    if (status == S32_MMIO_STATUS_ERR) return -1;
    return (int)status;
}

int s32_timer_cancel(int id) {
    unsigned int status = (unsigned int)s32_mmio_request(S32_MMIO_OP_TIMER_CANCEL, 0u, 0u, (unsigned int)id);
    return status == S32_MMIO_STATUS_ERR ? -1 : 0;
}

int s32_dpc_poll(s32_dpc_t *out) {
    unsigned int head = S32_MMIO_DPC_HEAD, tail = S32_MMIO_DPC_TAIL;
    if (head == tail) return 0;
    volatile uint32_t *ring = S32_MMIO_DPC_RING;
    unsigned int d = tail * S32_MMIO_DESC_WORDS;
    out->kind = ring[d + 0];
    out->length = ring[d + 1];
    out->id = ring[d + 2];
    out->cookie = ring[d + 3];
    S32_MMIO_DPC_TAIL = (tail + 1u) % S32_MMIO_DPC_ENTRIES;
    return 1;
}

int s32_dpc_wait(s32_dpc_t *out) {
    for (;;) {
        if (s32_dpc_poll(out)) return 1;
        unsigned int status = (unsigned int)s32_mmio_request(S32_MMIO_OP_POLL, 0u, 0u, 0u);
        if (status == S32_MMIO_STATUS_ERR) return -1;
    }
}

int s32_post_read(int fd, void *buf, unsigned n, unsigned cookie) {
    if (fd < 0 || !buf || n == 0) return -1;
    memcpy((void *)S32_MMIO_DATA_BUFFER, &cookie, 4);
    unsigned int status = (unsigned int)s32_mmio_request(S32_MMIO_OP_POST_READ, n,
                                                         (unsigned int)(unsigned long)buf,
                                                         (unsigned int)fd);
    return status == S32_MMIO_STATUS_ERR ? -1 : 0;
}

int s32_dpc_wait_on(const int *fds, unsigned nfds, s32_dpc_t *out) {
    if (nfds > S32_DPC_MAX_FDS) { errno = EINVAL; return -1; }
    for (;;) {
        if (s32_dpc_poll(out)) return 1;
        /* the fds go in the payload each time: the host keeps nothing
         * between one wait and the next */
        volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
        if (nfds) memcpy((void *)data_buffer, fds, nfds * sizeof(int));
        unsigned int status = (unsigned int)s32_mmio_request(S32_MMIO_OP_POLL, nfds * 4u, 0u, 0u);
        if (status == S32_MMIO_STATUS_ERR) return -1;
    }
}
