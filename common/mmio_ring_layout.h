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

// Opcode range tags help keep services grouped
enum {
    S32_MMIO_OPCODE_RANGE_CORE  = 0x00,  // Basic file/process I/O
    S32_MMIO_OPCODE_RANGE_MEM   = 0x10,  // Memory & process management
    S32_MMIO_OPCODE_RANGE_FS    = 0x20,  // Filesystem metadata
    S32_MMIO_OPCODE_RANGE_TIME  = 0x30,  // Time & timers
    S32_MMIO_OPCODE_RANGE_NET   = 0x40,  // Networking / IPC
    S32_MMIO_OPCODE_RANGE_HOST  = 0x60,  // Host services (env, randomness, etc.)
    S32_MMIO_OPCODE_RANGE_USER  = 0x80,  // Experimental / user-defined
};

// Operation codes shared between guest and host
enum s32_mmio_opcode {
    // 0x00 - 0x0F : Core process & stdio syscalls
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

    // 0x30 - 0x3F : Time & event services
    S32_MMIO_OP_GETTIME     = 0x30,  // Returns wall-clock seconds/nanoseconds
    S32_MMIO_OP_SLEEP       = 0x31,  // Sleep for N microseconds (future)
    S32_MMIO_OP_TIMER_START = 0x32,  // Arm timer, host completes on HP ring (future)
    S32_MMIO_OP_TIMER_CANCEL= 0x33,  // Cancel timer (future)
    S32_MMIO_OP_POLL        = 0x34,  // poll()/select()-style wait (future)

    // 0x40 - 0x4F : Networking / IPC
    S32_MMIO_OP_SOCKET      = 0x40,  // socket()
    S32_MMIO_OP_CONNECT     = 0x41,  // connect()
    S32_MMIO_OP_ACCEPT      = 0x42,  // accept()
    S32_MMIO_OP_SEND        = 0x43,  // send()/write() on socket
    S32_MMIO_OP_RECV        = 0x44,  // recv()/read() on socket
    S32_MMIO_OP_SHUTDOWN    = 0x45,  // shutdown()/close socket half
};

#endif // S32_MMIO_RING_LAYOUT_H
