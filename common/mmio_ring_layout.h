#ifndef S32_MMIO_RING_LAYOUT_H
#define S32_MMIO_RING_LAYOUT_H

#include <stdint.h>

// Offsets within the MMIO window (relative to __mmio_base)
enum {
    S32_MMIO_REQ_HEAD_OFFSET    = 0x0000u,
    S32_MMIO_REQ_TAIL_OFFSET    = 0x0004u,
    S32_MMIO_REQ_RING_OFFSET    = 0x1000u,
    S32_MMIO_RESP_HEAD_OFFSET   = 0x2000u,
    S32_MMIO_RESP_TAIL_OFFSET   = 0x2004u,
    S32_MMIO_RESP_RING_OFFSET   = 0x3000u,
    S32_MMIO_DATA_BUFFER_OFFSET = 0x4000u,
};

// Ring configuration constants
enum {
    S32_MMIO_RING_ENTRIES = 256u,
    S32_MMIO_DESC_WORDS   = 4u,
};

#define S32_MMIO_DESC_BYTES   (S32_MMIO_DESC_WORDS * sizeof(uint32_t))
#define S32_MMIO_DATA_CAPACITY (48u * 1024u)  // Total bytes available in data buffer

// Operation codes shared between guest and host
enum s32_mmio_opcode {
    S32_MMIO_OP_NOP     = 0x00,
    S32_MMIO_OP_PUTCHAR = 0x01,
    S32_MMIO_OP_GETCHAR = 0x02,
    S32_MMIO_OP_WRITE   = 0x03,
    S32_MMIO_OP_READ    = 0x04,
    S32_MMIO_OP_OPEN    = 0x05,
    S32_MMIO_OP_CLOSE   = 0x06,
    S32_MMIO_OP_SEEK    = 0x07,
    S32_MMIO_OP_BRK     = 0x08,
    S32_MMIO_OP_EXIT    = 0x09,
    S32_MMIO_OP_STAT    = 0x0A,
    S32_MMIO_OP_FLUSH   = 0x0B,
};

#endif // S32_MMIO_RING_LAYOUT_H