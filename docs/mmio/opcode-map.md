# MMIO Opcode Map

The ring descriptors double as Linux-style syscall packets. To keep them memorable, opcodes are grouped into fixed ranges:

| Range (hex) | Purpose                         | Notes |
|-------------|---------------------------------|-------|
| `0x00–0x0F` | Core process + stdio syscalls   | Current ops: `NOP`, `PUTCHAR`, `GETCHAR`, `WRITE`, `READ`, `OPEN`, `CLOSE`, `SEEK`, `BRK`, `EXIT`, `STAT`, `FLUSH`. |
| `0x10–0x1F` | Memory / process management     | Reserved for `MMAP`, `MUNMAP`, `SPAWN`, etc. |
| `0x20–0x2F` | Filesystem metadata & tooling   | Reserved for `FSTAT`, `UNLINK`, `DUP`, etc. |
| `0x30–0x3F` | Time, timers, and wait APIs     | Implemented: `GETTIME`. Planned: `SLEEP`, `TIMER_START`, `TIMER_CANCEL`, `POLL`. |
| `0x40–0x4F` | Networking / IPC                | Reserved identifiers for `SOCKET`, `CONNECT`, `ACCEPT`, `SEND`, `RECV`, `SHUTDOWN`, etc. |
| `0x60–0x7F` | Host services (env, randomness) | Reserved for `GETENV`, `RANDOM`, etc. |
| `0x80–0xFF` | Experimental / user-defined     | Safe playground for prototypes; no stability guarantees. |

## Descriptor schema (recap)

```
word0 = opcode
word1 = length      // primary byte count (payload)
word2 = offset      // data-buffer offset or secondary pointer index
word3 = status      // fd/flags for requests, errno/result for responses
```

Bulk arguments and structs live in `DATA_BUFFER`. Producers must write payloads first, then publish `REQ_HEAD`; consumers must read descriptors with load-acquire semantics before touching payloads.

## `GETTIME` Contract (0x30)

- **Request**: `length` ≥ 8, `offset` aligned so `{seconds, nanoseconds}` fits entirely in the data buffer.  
- **Response**: Writes two little-endian `uint32_t` words (`seconds`, `nanoseconds`) to `DATA_BUFFER[offset]`, sets `resp.length = 8`, and returns `resp.status = 0` on success. Errors clear `resp.length` and return `resp.status = 0xFFFFFFFF` (our current “-1” sentinel).
- **Usage**: libc can back `clock_gettime`, `time`, or `gettimeofday` by issuing this opcode and converting to the desired struct.
- **Runtime helper**: `runtime/time_mmio.c` wires this opcode into `clock_gettime()` and `time()` via the shared `s32_mmio_request()` helper so user programs can call standard libc APIs.

Future services should extend this file with their opcode IDs, payload expectations, and completion behavior so host and guest stay synchronized.
