# MMIO Opcode Map

The ring descriptors double as Linux-style syscall packets. To keep them memorable, opcodes are grouped into fixed ranges:

| Range (hex) | Purpose                         | Notes |
|-------------|---------------------------------|-------|
| `0x00–0x0F` | Core process + stdio syscalls   | Current ops: `NOP`, `PUTCHAR`, `GETCHAR`, `WRITE`, `READ`, `OPEN`, `CLOSE`, `SEEK`, `EXIT`, `STAT`, `FLUSH`. `0x08` reserved. |
| `0x10–0x1F` | Memory / process management     | Reserved for `MMAP`, `MUNMAP`, `SPAWN`, etc. |
| `0x20–0x2F` | Filesystem metadata & tooling   | Reserved for `FSTAT`, `UNLINK`, `DUP`, etc. |
| `0x30–0x3F` | Time, timers, and wait APIs     | Implemented: `GETTIME`, `SLEEP`. Planned: `TIMER_START`, `TIMER_CANCEL`, `POLL`. |
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

- **Request**: `length` ≥ 16 and `offset` aligned so the 16-byte `s32_mmio_timepair64_t` tuple fits entirely in the data buffer. The payload packs `seconds_lo`, `seconds_hi`, `nanoseconds`, and a reserved field for future flags.
- **Response**: Host writes the full tuple (`seconds_lo`/`seconds_hi` = 64-bit seconds, `nanoseconds` = 0..999,999,999, `reserved` = 0). `resp.length` is set to 16 and `resp.status = S32_MMIO_STATUS_OK` on success; errors clear `resp.length` and return `S32_MMIO_STATUS_ERR`.
- **Usage**: libc reconstructs the 64-bit seconds value and feeds `clock_gettime`, `time`, etc. This lifts the 2038 limitation for MMIO builds.
- **Runtime helper**: `runtime/time_mmio.c` wires this opcode into `clock_gettime()` and `time()` via the shared `s32_mmio_request()` helper so user programs can call standard libc APIs.

## `SLEEP` Contract (0x31)

- **Request**: `length` must be 16 and `offset` must point at an `s32_mmio_timepair64_t` describing the requested `nanosleep(2)` interval (64-bit seconds split across `seconds_hi/lo`, plus nanoseconds).
- **Response**:
  - Success → host zeroes the remainder, copies it back, sets `resp.length = 16`, and returns `resp.status = S32_MMIO_STATUS_OK`.
  - Interrupted (`EINTR`) → host copies the remaining time from `nanosleep(2)`, sets `resp.length = 16`, and returns `resp.status = S32_MMIO_STATUS_EINTR`.
  - Other errors → host clears `resp.length` and returns `resp.status = S32_MMIO_STATUS_ERR` (our sentinel `0xFFFFFFFF`).
- **Semantics**: Libc mirrors POSIX: `nanosleep()` returns `-1` when interrupted or on error, storing the pending interval in `rem` when provided. We still lack a global `errno`, so callers distinguish interruptions vs. fatal errors by examining whether a remainder was returned (`rem` only changes on EINTR).
- **Runtime helper**: `runtime/time_mmio.c` exposes `nanosleep()`, `usleep()`, and `sleep()` wrappers that speak this opcode. These helpers already understand `S32_MMIO_STATUS_EINTR`, so any caller that retries with the returned remainder will get Linux-like behavior.

## `STAT` Contract (0x0A)

- **Request**:
  - `fstat(fd)`: set `status = fd`, `length = 0`, and `offset` to the destination for the eventual result.
  - `stat(path)`: copy the NUL-terminated pathname into `DATA_BUFFER[offset]`, set `length = strlen(path)+1`, and write `status = S32_MMIO_STAT_PATH_SENTINEL`.
- **Response**: On success the emulator copies an `s32_mmio_stat_result_t` to `DATA_BUFFER[offset]`, sets `resp.length = sizeof(s32_mmio_stat_result_t)`, and returns `resp.status = S32_MMIO_STATUS_OK`. Errors clear `resp.length` and set `resp.status = S32_MMIO_STATUS_ERR`.
- **Result layout**:

| Field | Bits | Description |
|-------|------|-------------|
| `st_dev`, `st_ino`, `st_rdev` | 64 | Device/inode ids (zero-extended). |
| `st_mode` | 32 | POSIX mode bits (`S_IF*`, `S_IRUSR`, etc.). |
| `st_nlink`, `st_uid`, `st_gid` | 32 | Link count and owner ids. |
| `st_size`, `st_blksize`, `st_blocks` | 64 | Size in bytes, preferred block size, allocated blocks. |
| `st_atime_sec/nsec`, `st_mtime_sec/nsec`, `st_ctime_sec/nsec` | 64+32 each | Timestamps with nanosecond precision. |

- **Runtime helper**: `runtime/stat_mmio.c` exposes `stat()`/`fstat()` that copy the packed structure into the public `struct stat`. When linking against `libc_debug.s32a`, tiny stubs return `-1` so binaries still link, albeit without metadata.

Future services should extend this file with their opcode IDs, payload expectations, and completion behavior so host and guest stay synchronized.
