# The input queue

How a SLOW-32 program hears the world, and how it does not.

Captured 2026-11-18, after the question of whether the desk should
grow a windowing framework. Sibling to [hose.md](hose.md),
[tube.md](tube.md), [dpc.md](dpc.md), and [hosting.md](hosting.md).
The wire stays where it already is: [SPEC.md](../SPEC.md) §8.12–8.16
and [TUBE.md](../TUBE.md) §2. This file does not add an opcode.

## The sentence

The guest owns the thread. The host never calls in. An event is a
record the guest copies out when it next looks.

`_start` calls `main`, and `main` returns. That is the process. A
desk program still has a loop inside `main`. The loop asks. It is
not asked.

## What is already a queue

Three devices, on purpose. They are not one stream.

| Device | Guest ask | Record | Wait |
|---|---|---|---|
| Stdin / term | `GETCHAR`, `read` on fd 0, `term_getkey`, `term_getchar` | one byte, or one UTF-8 code point | blocks. `term_kbhit` does not. `term_wait_key(ms)` waits in the host |
| Tube keys | `tube_keys` / `TUBE_KEYS` | 4 bytes: `u16 code`, `u8 down`, `u8 reserved` | never blocks. Count 0 means empty |
| DPC ring | `s32_dpc_poll` / `s32_dpc_wait` | `s32_dpc_t`: kind, length, id, cookie | `poll` does not block. `wait` sleeps until a timer or a posted read can complete |

Readiness of an fd is a fourth shape, `FDPOLL`: the guest names the
fds and a timeout, the host writes `revents` in place. A posted read
(`s32` post) is a DPC whose bytes are already in the caller's buffer
when the entry is harvested.

Term input is host stdin (the prefix file, then `read(2)`). Tube
keys are the viewer queue only. [TUBE.md](../TUBE.md) says they are
not merged, and that stays. A program that wants both has two
devices, the way a 1987 machine had a keyboard and a serial port.

Tube queue cap is 256 events; overflow drops the oldest. Leftover
events stay queued. A term read consumes the byte. A DPC harvest
consumes the entry; `s32_dpc_unread` puts one back for a waiter that
is not the sole consumer. Stale timers are dropped, not unread.

End of stdin is a status, not a hang: term `READ_KEY` / `READ_CHAR`
return `0xFFFFFFFD`. Tube with no viewer and no injection returns
zero events. `s32_dpc_wait` returns `EAGAIN` when nothing is armed,
so a wait cannot be a sleep with no alarm.

## The grammar

Every record has a source, a kind, and a payload. Today the source
is which call you made.

- **Stdin byte.** Cooked or raw, depending on term `SET_MODE`.
  `READ_KEY` is one byte. Escape sequences arrive a byte at a time,
  ESC first.
- **Stdin character.** `READ_CHAR` is one Unicode scalar, or U+FFFD
  for a broken sequence. Still the keyboard, still not the tube.
- **Tube key.** Make and break. Printable ASCII is itself. Extended
  codes start at `0x100` (`UP`, `DOWN`, `LEFT`, `RIGHT`, shifts,
  controls, alts, `F1`–`F12`). Unknown codes are still queued; the
  guest decides. `down` is 0 or 1. `reserved` is 0 on write and
  ignored on read.
- **DPC.** A timer that fired (`id`, `cookie`) or a posted read that
  finished (`id` is the guest buffer, `length` is the byte count,
  `cookie` is what the guest posted). A readiness DPC is the same
  struct with kind `POLL`.
- **Poll slot.** An fd the guest named, plus the bits the host found
  (`IN`, `HUP`, `ERR`, `NVAL`).

Nothing in that list is a function pointer. Nothing in that list
runs because the host felt like it. Translated code runs until the
guest hits YIELD, HALT, DEBUG, or a fault
([hosting.md](hosting.md)). The host fills rings only at those
points.

## The loop

Two legal shapes.

**Batch.** Read what is there, compute, write, return. Fortran,
filters, `s32run hello.s32x`. No queue discipline beyond EOF.

**Desk.** A loop that asks, updates, and draws. nano, BROWSE,
`@GET`, Asteroids, Doom. The loop is the program. A nested loop is
a command: dBase `GET`, WordStar `^K`, a prompt that reads until
Enter. It has an entry and an exit. When it returns, the outer loop
is where it was. That is how a character-mode program stays
writable. A dispatcher that owns `main` and calls the program back
is the thickness this machine refuses.

Pacing is the guest's. The tube has no vsync and no guest timer
interrupt. `GETTIME` and `SLEEP` are host wall clock and host
`nanosleep`; they are not deterministic. A frame loop that must wake
arms `s32_timer_start` and waits on the DPC ring, or calls `SLEEP`.
Tests do not sit on a human: `S32_STDIN_PREFIX` feeds stdin,
`S32_TUBE_KEYS` preloads the 4-byte records at `OPEN`,
`S32_TERM_KEYMARK` marks each term read so a harness can cut a
screen per key.

## The glass

The viewer may be as event-driven as it likes. `s32-crt` and
`s32-crt-mac` sit in a host loop, turn NSEvent or terminal bytes
into `KEYE` frames or into stdin, and reattach when a new guest
appears. That thickness stays in native code. The cartridge does
not link AppKit.

Absence is a legal answer. Headless `OPEN` still succeeds. No
viewer, no injection: the key queue is empty and `PRESENT` still
journals. `--deny term` and `--deny tube` are how a batch guest
declines a screen.

## What not to build

- A guest interrupt, a vector, or a callback into guest code. A
  host event becomes a record, or it is dropped. See
  [thread-service-routines.md](../thread-service-routines.md): the
  design that lost.
- A widget toolkit, a responder chain, or a guest `RunLoop`. The
  1987 desk is nested loops over a screen and a file. A SLOW-32
  Cocoa is someone else's run loop with extra steps.
- One merged keyboard. Term and tube stay separate devices. Merging
  them would hide which one the program asked.
- A mouse, a menu bar, or a window id, until a program cannot be
  written without it. The tube amendment already says a mouse has
  to earn its way in ([tube.md](tube.md)). If it does, it is another
  record on the tube queue (`code`, `down`, and two coordinates in
  a longer event), not a new framework.
- An opcode that sleeps until "anything." `term_wait_key` already
  waits on stdin. `s32_dpc_wait` already waits on the DPC ring.
  `FDPOLL` already waits on fds. A guest that wants a key *or* a
  timer asks both, or arms a short timer and drains the tube queue
  when it fires. One select-everything opcode waits for a program
  that cannot say which device it is waiting on.

## Injection is the test contract

A queue you cannot fill from a file is not a queue this tree will
keep. Stdin has `S32_STDIN_PREFIX`. Tube keys have `S32_TUBE_KEYS`
and `KEYE`. DPC timers fire from `s32_timer_start` under the test's
own clock only as far as the host clock allows; scripted guests
prefer a prefix file and a finite loop over a wall-clock sleep.

The differential harnesses compare engines on bytes. A new record
kind is a new expected file, not a new thread.
