#include <stdint.h>

#include "errno.h"
#include "mmio_ring.h"

extern void yield(void);

static unsigned int next_index(unsigned int value) {
    return (value + 1u) % S32_MMIO_RING_ENTRIES;
}

static void set_errno_for_status(unsigned int status) {
    if (status == S32_MMIO_STATUS_EINTR) {
        errno = EINTR;
    } else if (status == S32_MMIO_STATUS_ERR) {
        errno = EIO;
    }
}

int s32_mmio_request(unsigned int opcode,
                     unsigned int length,
                     unsigned int offset,
                     unsigned int status) {
    volatile uint32_t *req_ring = S32_MMIO_REQ_RING;
    volatile uint32_t *resp_ring = S32_MMIO_RESP_RING;

    unsigned int req_head = S32_MMIO_REQ_HEAD;
    unsigned int req_tail = S32_MMIO_REQ_TAIL;

    while (next_index(req_head) == req_tail) {
        yield();
        req_tail = S32_MMIO_REQ_TAIL;
    }

    unsigned int desc = req_head * S32_MMIO_DESC_WORDS;
    req_ring[desc + 0] = opcode;
    req_ring[desc + 1] = length;
    req_ring[desc + 2] = offset;
    req_ring[desc + 3] = status;

    S32_MMIO_REQ_HEAD = next_index(req_head);

    unsigned int resp_head = S32_MMIO_RESP_HEAD;
    unsigned int resp_tail = S32_MMIO_RESP_TAIL;

    while (resp_head == resp_tail) {
        yield();
        resp_head = S32_MMIO_RESP_HEAD;
        resp_tail = S32_MMIO_RESP_TAIL;
    }

    unsigned int resp_desc = resp_tail * S32_MMIO_DESC_WORDS;
    unsigned int result = resp_ring[resp_desc + 3];

    S32_MMIO_RESP_TAIL = next_index(resp_tail);
    set_errno_for_status(result);
    return (int)result;
}
