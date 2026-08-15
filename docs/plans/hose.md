# The hose

How two SLOW-32 programs talk, and how they do not.

Captured 2026-08-15 after the 1987-desk conversation asked the
obvious question: Kermit is a file protocol for a wire — what is
the other end, if there is no serial port?

## The answer

A serial port, a TCP socket, and a null-modem cable are the same
MMIO shape: `open` / `read` / `write` / `poll` / `close`. The guest
cannot tell whether the other end is another SLOW-32, a Unix
socket, TCP, a PTY, or a host `socketpair`. That is the point.

Kermit (and X/Y/ZMODEM) only earn their keep when the two sides
**do not share a disk**. Talking ZMODEM into the same MMIO
filesystem the guest already has is a costume.

## What not to start with

- Guest-to-guest shared memory. Guest RAM is already host RAM.
  Mapping the same pages into two guests couples crash domains,
  needs agreed addresses, and still needs a doorbell (no
  interrupts). Character-mode programs were waiting on a byte pipe
  and a directory of files.
- A guest that boots the cluster. A guest that can spawn arbitrary
  `.s32x` is a confused deputy. The host already is the OS.
- DNS, IPv6, UDP, or a BSD stack. Name-to-IP is a later service.
- In-process threads sharing one emulator. One bug kills the desk.
  Start as separate processes.

## Bootstrap

The host composes machines. Guests only see hoses and files.

1. **Today:** implement the reserved socket opcodes. Two
   `slow32` processes, IPv4 numeric addresses (`127.0.0.1`). A
   shell script is the cluster file.
2. **Next:** a desk file (`desk.toml` or a stanza file) that
   starts N emulators and pre-wires `socketpair`s. Exactly one
   guest owns the terminal, or none (daemon cluster). Capabilities
   are per guest.
3. **`exec` service** — **v1 landed**: `S32_MMIO_OP_EXEC` (0x10)
   forks the same emulator with `-q` and waits. Image must be a
   `.s32x`. Policy name `exec`. `status = 0xFFFFFFFF` inherits
   stdio (COMMAND.COM). `status = guest_fd` dups that host fd
   onto the child's 0/1/2 (BBS doors).
4. **ganl** as the supervisor event loop only when a listener and
   Telnet are real (humans dial in, NAWS, binary mode for ZMODEM).
   Steal `NetworkEngine`, `adoptConnection`, `spawnSlave`, the
   telnet handler, and the close-ownership contract. Do not steal
   the MUD session layer — user records belong in guest dBase.
5. **Guest-to-guest shm** only after a program has a reason a pipe
   cannot express.

JSON vs YAML vs TOML is not the interesting question. The host
reads a small manifest. Guests do not.

## v1 (landed)

IPv4 TCP only. Opcodes `0x40–0x48` in `common/mmio_ring_layout.h`.
Guest libc in `runtime/net_mmio.c`. Host handling in
`tools/emulator/mmio_ring.c` (covers `slow32`, `slow32-fast`,
`slow32-dbt`) and `qemu-backend/target/slow32/mmio.c`.

- `socket` / `bind` / `listen` / `accept` / `connect` / `shutdown`
- `getsockname` so bind-to-port-0 works
- `send` / `recv` are `write` / `read` on the guest fd
- `inet_aton` / `inet_addr` / `htonl` — dotted quads, no names
- Policy name `net`; default allow

Out of v1: DNS, UDP, Unix sockets, `poll`, non-blocking, `getpeername`,
options other than implicit `SO_REUSEADDR` on bind.

See `net/README.md` for the two-guest echo, and
`regression/tests/feature-socket-basic/` for the single-process
smoke test.

Selfhost copies of `mmio_ring_*.c` are **not** updated in this
slice. They grow the same opcodes when those emulators next need
a hose.
