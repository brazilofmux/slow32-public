# MMIO Opcode Map

Authority: `common/mmio_ring_layout.h`. This file is the human registry.
A new opcode lands in the header first, then here.

The ring descriptors double as Linux-style syscall packets. Opcodes
are grouped into fixed ranges. **None of them is an interrupt.** A
timer that fires is a DPC — a queue entry the instance reads when it
looks — not a vector into translated code. That split is why the DBT
can chain superblocks until YIELD and not apologize; see
[plans/dpc.md](../plans/dpc.md) and [plans/hosting.md](../plans/hosting.md).

| Range (hex) | Purpose | Notes |
|---|---|---|
| `0x00–0x0F` | Core process + stdio | `NOP`, `PUTCHAR`, `GETCHAR`, `WRITE`, `READ`, `OPEN`, `CLOSE`, `SEEK`, `EXIT` (0x09), `STAT`, `FLUSH`, `READ_DIRECT`, `FTRUNCATE`, `POST_READ` (0x0E). `0x08` reserved (was `BRK`). |
| `0x10–0x1F` | Process management | `EXEC` (0x10): run a `.s32x` and wait. |
| `0x20–0x2F` | Filesystem metadata | `UNLINK`, `RENAME`, `MKDIR`, `RMDIR`, `LSTAT`, `ACCESS`, `CHDIR`, `GETCWD`, `OPENDIR`, `READDIR`, `CLOSEDIR`, `REWINDDIR`. |
| `0x30–0x3F` | Time, timers, wait | `GETTIME`, `SLEEP`, `TIMER_START`, `TIMER_CANCEL`, `POLL` (DPC wait), `GETTZ`. |
| `0x40–0x4F` | Networking / IPC | IPv4 TCP v1: `SOCKET` … `GETSOCKNAME`. `SEND`/`RECV` alias `WRITE`/`READ`. No DNS, no UDP, no Unix sockets. |
| `0x60–0x7F` | Host environment | `ARGS_INFO`/`ARGS_DATA`, `ENVP_INFO`/`ENVP_DATA`, `GETENV`. |
| `0x80–0xEF` | Negotiated services | `term`, `tube`, and anything granted by `SVC_REQUEST`. Guest-picked bases. |
| `0xF0–0xFF` | Service negotiation | `SVC_REQUEST`, `SVC_RELEASE`, `SVC_QUERY`, `SVC_LIST`, `SVC_VERSION`. |

## Descriptor schema (recap)

```
word0 = opcode
word1 = length      // primary byte count (payload)
word2 = offset      // data-buffer offset or secondary pointer index
word3 = status      // fd/flags for requests, errno/result for responses
```

Bulk arguments and structs live in `DATA_BUFFER`. Producers must write
payloads first, then publish `REQ_HEAD`; consumers must read
descriptors with load-acquire semantics before touching payloads.

The **DPC ring** is a third ring, host→guest, in the page below the
request ring: head/tail at `0x0010`/`0x0014`, 64 entries at `0x0800`.
It is not the response ring. A DPC is `{opcode, length, offset, status}`
like any other descriptor. For a timer: opcode `TIMER_START`, length 0,
offset = timer id, status = the cookie the guest armed it with.

## `EXEC` (0x10)

Run another `.s32x` in a child emulator and wait. Path and argv in the
data buffer. `status = 0xFFFFFFFF` inherits stdio (COMMAND.COM).
`status = guest_fd` dups that host fd onto the child's 0/1/2 (BBS
doors). Policy name `exec`. See [plans/hose.md](../plans/hose.md).
Spawn-without-wait (exit status later, as a DPC) is not here yet.

## Filesystem metadata (`0x20–0x2B`)

| Opcode | |
|---|---|
| `UNLINK` (0x20) | delete a file |
| `RENAME` (0x21) | rename/move |
| `MKDIR` (0x22) / `RMDIR` (0x23) | directories |
| `LSTAT` (0x24) | `stat` without following symlinks |
| `ACCESS` (0x25) | accessibility; mode constants in the header |
| `CHDIR` (0x26) / `GETCWD` (0x27) | working directory |
| `OPENDIR` (0x28) / `READDIR` (0x29) / `CLOSEDIR` (0x2A) / `REWINDDIR` (0x2B) | directory streams |

`READDIR` writes an `s32_mmio_dirent_t`. `STAT`/`LSTAT` write an
`s32_mmio_stat_result_t` (see `STAT` below).

## `GETTIME` Contract (0x30)

- **Request**: `length` ≥ 16 and `offset` aligned so the 16-byte
  `s32_mmio_timepair64_t` tuple fits entirely in the data buffer. The
  payload packs `seconds_lo`, `seconds_hi`, `nanoseconds`, and a
  reserved field for future flags.
- **Response**: Host writes the full tuple (`seconds_lo`/`seconds_hi` =
  64-bit seconds, `nanoseconds` = 0..999,999,999, `reserved` = 0).
  `resp.length` is set to 16 and `resp.status = S32_MMIO_STATUS_OK` on
  success; errors clear `resp.length` and return `S32_MMIO_STATUS_ERR`.
- **Usage**: libc reconstructs the 64-bit seconds value and feeds
  `clock_gettime`, `time`, etc. This lifts the 2038 limitation for
  MMIO builds.
- **Runtime helper**: `runtime/time_mmio.c` wires this opcode into
  `clock_gettime()` and `time()` via the shared `s32_mmio_request()`
  helper so user programs can call standard libc APIs.

## `SLEEP` Contract (0x31)

- **Request**: `length` must be 16 and `offset` must point at an
  `s32_mmio_timepair64_t` describing the requested `nanosleep(2)`
  interval (64-bit seconds split across `seconds_hi/lo`, plus
  nanoseconds).
- **Response**:
  - Success → host zeroes the remainder, copies it back, sets
    `resp.length = 16`, and returns `resp.status = S32_MMIO_STATUS_OK`.
  - Interrupted (`EINTR`) → host copies the remaining time from
    `nanosleep(2)`, sets `resp.length = 16`, and returns
    `resp.status = S32_MMIO_STATUS_EINTR`.
  - Other errors → host clears `resp.length` and returns
    `resp.status = S32_MMIO_STATUS_ERR` (our sentinel `0xFFFFFFFF`).
- **Semantics**: Libc mirrors POSIX: `nanosleep()` returns `-1` when
  interrupted or on error, storing the pending interval in `rem` when
  provided. We still lack a global `errno`, so callers distinguish
  interruptions vs. fatal errors by examining whether a remainder was
  returned (`rem` only changes on EINTR).
- **Runtime helper**: `runtime/time_mmio.c` exposes `nanosleep()`,
  `usleep()`, and `sleep()` wrappers that speak this opcode.

`SLEEP` is “block this instance for an interval.” It is not a timer
interrupt, and it is not how a reactor waits for *either* a timer *or*
a hose. That wait is `POLL`.

## `POST_READ` (0x0E)

A read that completes as a DPC, not as this request’s response. The
instance posts a flow and keeps its stack; it does not become a
thread parked in `read()`. Policy name `fs`.

- **Request**: `status` = fd, `length` = max bytes, `offset` = dest in
  `DATA_BUFFER`. The guest writes a 32-bit cookie into the first four
  bytes at dest; the host saves it and then reads into dest. dest must
  not overlap the stdio bounce at offset 0 while other MMIO is in
  flight — that buffer is shared scratch, not per-flow memory.
- **Response**: `status = OK` if the flow was taken. The bytes are
  **not** in this response. If the fd would block, `ERR`/`EAGAIN` and
  no DPC — we refuse the flow rather than sit on the fd.
- **DPC**: `{opcode POST_READ, length nbytes, offset dest, status cookie}`.
  Queued at this service point. The dest bytes are host-owned until
  the guest harvests that DPC.
- **Runtime**: `s32_post_read` in `s32dpc.h`. Test:
  `regression/tests/feature-dpc-post-read`.

This is the second DPC demo ([plans/dpc.md](../plans/dpc.md)): a
request that needs a reply comes back through the queue. There is
still no helper thread writing into a running guest.

## Timers and `POLL` (0x32–0x34)

These are implemented. They are not a guest IRQ, and they were not
added so Xinu could tick. Bare metal would take a timer interrupt in
**native** code and that handler would enqueue a DPC; the instance
would never see the IRQ. Hosted, the emulator is that native code.
[plans/dpc.md](../plans/dpc.md) is the design; `runtime/include/s32dpc.h`
is the guest API; `regression/tests/feature-dpc-timer` runs it on all
four engines.

| Opcode | Request | Response / DPC |
|---|---|---|
| `TIMER_START` (0x32) | `offset` → `timepair64` interval; `status` = guest cookie | `resp.status` = timer id `0 .. TIMER_MAX-1`, or `ERR`/`EAGAIN` if the partition is full. When the interval elapses, a DPC `{TIMER_START, 0, id, cookie}` is queued **at the next service point** (or during a `POLL`). The id is then free. |
| `TIMER_CANCEL` (0x33) | `status` = id | A cancelled timer never queues. |
| `POLL` (0x34) | none | Sleep until the DPC ring is non-empty. `resp.status` = entries waiting. `ERR`/`EAGAIN` if the ring is empty **and** nothing is armed, so an instance is never left asleep for something that cannot come. |

`S32_MMIO_TIMER_MAX` is 8: a fixed partition. `POLL` here is “sleep
until my DPC ring has something,” not POSIX `poll(2)` on guest fds.
That wait is still future work ([plans/hosting.md](../plans/hosting.md)
level 1).

Delivery is at service points only. The host does not write a DPC
into a running guest. A timer that expires while the instance is in
translated code is queued at its next YIELD.

## `GETTZ` (0x35)

Guest writes a `timepair64` (the UTC instant to convert) at
`req->offset`. Host overwrites the same 16 bytes with
`s32_mmio_tzinfo_t`: `gmtoff_sec`, `is_dst`, `abbrev[8]`.

## `STAT` Contract (0x0A)

- **Request**:
  - `fstat(fd)`: set `status = fd`, `length = 0`, and `offset` to the
    destination for the eventual result.
  - `stat(path)`: copy the NUL-terminated pathname into
    `DATA_BUFFER[offset]`, set `length = strlen(path)+1`, and write
    `status = S32_MMIO_STAT_PATH_SENTINEL`.
- **Response**: On success the emulator copies an
  `s32_mmio_stat_result_t` to `DATA_BUFFER[offset]`, sets
  `resp.length = sizeof(s32_mmio_stat_result_t)`, and returns
  `resp.status = S32_MMIO_STATUS_OK`. Errors clear `resp.length` and
  set `resp.status = S32_MMIO_STATUS_ERR`.
- **Result layout**:

| Field | Bits | Description |
|---|---|---|
| `st_dev`, `st_ino`, `st_rdev` | 64 | Device/inode ids (zero-extended). |
| `st_mode` | 32 | POSIX mode bits (`S_IF*`, `S_IRUSR`, etc.). |
| `st_nlink`, `st_uid`, `st_gid` | 32 | Link count and owner ids. |
| `st_size`, `st_blksize`, `st_blocks` | 64 | Size in bytes, preferred block size, allocated blocks. |
| `st_atime_sec/nsec`, `st_mtime_sec/nsec`, `st_ctime_sec/nsec` | 64+32 each | Timestamps with nanosecond precision. |

- **Runtime helper**: `runtime/stat_mmio.c` exposes `stat()`/`fstat()`
  that copy the packed structure into the public `struct stat`. When
  linking against `libc_debug.s32a`, tiny stubs return `-1` so binaries
  still link, albeit without metadata.

## Sockets (v1, IPv4 TCP only)

No DNS, no UDP, no Unix sockets. The guest libc (`runtime/net_mmio.c`)
speaks POSIX `sockaddr_in` (network byte order) and packs an 8-byte
`s32_mmio_sockaddr_in_t` for the host: `addr` and `port` in guest
endian, `family = 2` (`S32_AF_INET`).

| Opcode | `status` in | payload | `status` out |
|---|---|---|---|
| `SOCKET` (0x40) | `family \| type<<8 \| proto<<16` | none | new guest fd |
| `CONNECT` (0x41) | fd | `s32_mmio_sockaddr_in_t` | OK |
| `ACCEPT` (0x42) | listen fd | peer written back | new guest fd |
| `SEND` (0x43) | fd | bytes (alias of `WRITE`) | byte count |
| `RECV` (0x44) | fd | bytes (alias of `READ`) | byte count |
| `SHUTDOWN` (0x45) | fd | `length` = how (0/1/2) | OK |
| `BIND` (0x46) | fd | `s32_mmio_sockaddr_in_t` | OK |
| `LISTEN` (0x47) | fd | `length` = backlog | OK |
| `GETSOCKNAME` (0x48) | fd | address written back | OK |

`SOCKET` accepts only `family=2`, `type=1` (`SOCK_STREAM`), protocol
0 or 6. The host sets `SO_REUSEADDR` on `BIND`. `CLOSE` already
closes the guest fd. Policy name is `net` (legacy opcode range
`0x40–0x4F`); default policy allows it.

Unix `socketpair` as a same-machine hose, and POSIX `poll` on guest
fds, are hosting work, not new families here.

## Host environment (`0x60–0x64`)

| Opcode | |
|---|---|
| `ARGS_INFO` (0x60) / `ARGS_DATA` (0x61) | guest `argv[]` |
| `ENVP_INFO` (0x62) / `ENVP_DATA` (0x63) | guest `environ[]` |
| `GETENV` (0x64) | one variable by name |

Structs `s32_mmio_args_info_t` / `s32_mmio_envp_info_t` in the header.

## Service negotiation (`0xF0–0xF4`)

See [SERVICE_NEGOTIATION.md](../SERVICE_NEGOTIATION.md). `term` and
`tube` live in the negotiated range, not in the fixed table.

Future services extend this file with opcode IDs, payload
expectations, and completion behavior so host and guest stay
synchronized. New *interrupt-shaped* guest behaviour does not.
