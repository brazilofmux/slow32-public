#include <stdlib.h>

#include "mmio_ring.h"

extern void yield(void);
extern void __cxa_finalize(void *dso_handle);

void exit(int status) {
    __cxa_finalize(0);
    unsigned int req_head = S32_MMIO_REQ_HEAD;
    unsigned int req_tail = S32_MMIO_REQ_TAIL;
    volatile unsigned int *req_ring = S32_MMIO_REQ_RING;

    while (((req_head + 1u) % S32_MMIO_RING_ENTRIES) == req_tail) {
        yield();
        req_tail = S32_MMIO_REQ_TAIL;
    }

    unsigned int idx = req_head * S32_MMIO_DESC_WORDS;
    req_ring[idx + 0] = S32_MMIO_OP_EXIT;
    req_ring[idx + 1] = 0;
    req_ring[idx + 2] = 0;
    req_ring[idx + 3] = (unsigned int)status;

    S32_MMIO_REQ_HEAD = (req_head + 1u) % S32_MMIO_RING_ENTRIES;

    while (1) yield();
}

void abort(void) {
    exit(EXIT_FAILURE);
}
