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

`s32_dpc_wait_on` joins the two. It sleeps until a timer fires, a
posted read completes, or one of the named fds is readable, and a
readable fd arrives on the DPC ring like everything else (kind
`POLL`, spelled `S32_DPC_READY` in `s32dpc.h`). Stdin is fd 0, so
"a key or a timeout" on the terminal is already one wait. The tube
key queue is not an fd. That is the second edge below.

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

## One thread, a task heap, and the queue

The desk loop grown up: one thread that runs scheduled work *and*
answers input, with nothing calling in. A Win32 program does this
with `MsgWaitForMultipleObjects` and its timeout set to the next
deadline. Here the same loop is three calls.

Scheduled work lives in a min-heap in the guest's own memory, keyed
on when it is due, so the next job is always on top. The host never
sees the heap. It sees one timer, armed for the top.

```c
for (;;) {
    while (nheap > 0 && heap[0].due <= now_ns()) {
        job_t j = pop();                    /* run what is due; a job may push more */
        j.run(j.arg);
    }
    if (nheap > 0 && (timer < 0 || heap[0].due != armed_for)) {
        if (timer >= 0) s32_timer_cancel(timer);    /* one already queued goes stale */
        long long d = heap[0].due - now_ns();
        if (d < 0) d = 0;
        timer = s32_timer_start(d / 1000000000LL, d % 1000000000LL, ++cookie);
        armed_for = heap[0].due;
    }
    s32_dpc_t e;
    if (s32_dpc_wait_on(fds, nfds, &e) < 0)
        break;                              /* EAGAIN: no timer, no fd -- nothing can arrive */
    if (e.kind == S32_DPC_TIMER) {
        if (e.cookie == cookie) timer = -1; /* ours fired; due jobs run at the top */
        continue;                           /* a stale one is dropped, not unread */
    }
    if (e.kind == S32_DPC_READY && e.id == 0) {
        char buf[80];
        int n = read(0, buf, sizeof buf);
        if (n <= 0) { nfds = 0; continue; } /* end of input: stop watching it */
        /* ... handle the input ... */
    }
}
```

`now_ns` is `clock_gettime(CLOCK_MONOTONIC)`; `push` and `pop` are an
ordinary binary heap. Built with clang and run with a line arriving
on stdin between two jobs, it prints the same interleaving on the
interpreter, `slow32-fast` and the DBT, and ends by itself when the
heap is empty and stdin is closed.

Four things make it work, and each is already in the tree:

- **One host timer, any number of jobs.** The host gives a program
  eight timers (`S32_MMIO_TIMER_MAX`), a fixed partition. The heap
  is why eight is enough: the program needs one, for the top. A
  program that asks for more timers wants a heap.
- **Stale timers by cookie.** Re-arming does not race the old timer.
  Each arm carries a new cookie; an entry with an old one is dropped.
  Kermit does the same for its packet timeout (`kermit.c`).
- **The end is a status.** With stdin closed and the heap empty,
  nothing is armed and nothing is watched, so `s32_dpc_wait_on`
  answers `EAGAIN` and the loop ends. It cannot sleep forever.
- **Input is a record.** The readable fd arrives on the same ring as
  the timer. The program reads it when it chooses, between jobs, so
  a job is never interrupted halfway.

**Beside `s32sched.h`.** The tree's cooperative scheduler
(`runtime/include/s32sched.h`, test `feature-dpc-sched`) answers a
different question: several tasks, each a protothread that awaits
I/O or a timer, so one task's computation overlaps another's I/O.
It spends a host timer on each task that awaits one, which is why
it stops at eight tasks. The heap answers "many things due at
different times, one thread". The two compose: a scheduler that kept
its timer waits on a heap would arm one host timer for all of them.

**The server: TinyMUX.** The same shape, at the size of a real
network server, is `GanlAdapter::run_main_loop()` in
[TinyMUX](https://github.com/brazilofmux/tinymux)
(`mux/src/ganl_adapter.cpp`). Each pass asks the scheduler
`WhenNext`, makes that the timeout, waits in ganl's
`processEvents` (epoll, kqueue or IOCP underneath), dispatches the
network events, and then calls `RunTasks`. The scheduler
(`CScheduler`, `mux/modules/engine/timer.cpp`) is the heap above,
grown up, and it adds two lessons:

- *Every deadline shortens the wait.* TinyMUX issue 1074 was a
  deadline outside the heap: the grace period for a new client to
  show its protocol. `WhenNext` did not know of it, so the server
  could sleep for minutes and a silent client was never aged out.
  The fix shortens the timeout by hand. Here the rule is simpler:
  a deadline goes in the heap, or it is not kept.
- *Due is not next.* `CScheduler` has two heaps. `ReadyTasks` moves
  what is due from the time heap to a priority heap, and `RunTasks`
  takes at most `active_q_chunk` tasks from it per pass; while ready
  tasks remain, `WhenNext` answers "now", so the loop comes straight
  back. A long queue of commands cannot starve the network. The
  one-heap loop above runs everything due before it looks at input
  again: right for a desk program, wrong for a server.

It is also more than an analogy. netmux built for SLOW-32 serves
telnet under the DBT (2026-10-31). There ganl uses its `select`
engine, the guest's `select` is `FDPOLL`, and the timeout `WhenNext`
computed is `FDPOLL`'s timeout: this loop, on this machine, without
a callback anywhere.

**And the edges (below).** Edge 2 is what this loop is missing for a tube
program: put the tube key queue in `fds` and a game has the same
loop. Edge 1 is what makes the loop a test: under the step clock,
the order in which jobs run and input is answered is the same on
every engine, so a desk program with a scheduler can be held to
bytes.

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
  `FDPOLL` already waits on fds, and `s32_dpc_wait_on` waits on fds
  and the ring together. A guest names what it is waiting for. One
  select-everything opcode waits for a program that cannot say which
  device it is waiting on. The one source `s32_dpc_wait_on` cannot
  name yet is the tube key queue; the second edge below gives it a
  name rather than an opcode.

## Injection is the test contract

A queue you cannot fill from a file is not a queue this tree will
keep. Stdin has `S32_STDIN_PREFIX`. Tube keys have `S32_TUBE_KEYS`
and `KEYE`. Time has nothing yet: DPC timers, `GETTIME` and `SLEEP`
follow the host's clocks, so scripted guests prefer a prefix file
and a finite loop over a wall-clock sleep. The first edge below is
the clock a test can own.

The differential harnesses compare engines on bytes. A new record
kind is a new expected file, not a new thread.

## Three edges

Places where the rules above hold but the machine does not yet let
a program, or a test, do the obvious thing. Each is proposed, not
built. None adds an opcode, a callback, or a merged device.

### 1. Time is the one input a test cannot inject

**The gap.** Keys and stdin come from files. Time does not.
`GETTIME` and `SLEEP` use the host's clocks ([SPEC.md](../SPEC.md)
§8.8: "not deterministic"), and DPC timers expire on them. So a
desk program that mixes input with time cannot be held to bytes:
Asteroids' frame loop (`usleep` between frames), libcob telling a
lone ESC from an escape sequence by `term_wait_key(SCR_ESC_MS)`,
Kermit resending a packet when its timer fires. The differential
harnesses stop at exactly the programs this document is about.

**The proposal.** An opt-in clock that advances at service points,
not with the host. `S32_CLOCK=step` in the environment, beside
`S32_STDIN_PREFIX` and `S32_TUBE_KEYS`:

- Time starts at a fixed instant (`S32_CLOCK_EPOCH`, seconds since
  the POSIX epoch; a fixed default), so a program's dates are the
  same on every run and every host.
- Every service point advances it by one quantum
  (`S32_CLOCK_STEP`, nanoseconds; default 1 ms). A loop that spins
  on `GETTIME` waiting for a second still ends, after a thousand
  service points.
- `SLEEP` advances it by the interval and returns at once.
- A wait with nothing ready (`s32_dpc_wait`, `s32_dpc_wait_on`,
  `FDPOLL`, `term_wait_key`) jumps it to the earliest armed timer's
  deadline, or to the wait's own timeout, and goes on. A wait with
  nothing armed and no timeout is `EAGAIN` or EOF, as it already is.
- Every clock `GETTIME` offers (wall, monotonic, processor) reads
  the one virtual clock. `GETTZ` still answers in the host's zone;
  a test that prints local time sets `TZ` as well.

**Why service points and not instructions.** dos-monster's
`X86_VCLOCK` counts instructions. That works for one engine. Here
the DBT does not count retired instructions, and the four engines
would disagree. Service points they agree on already: the same
program with the same injected input makes the same requests in the
same order on every engine. That is what the harnesses compare.

**What it does not do.** It does not make a live socket or a human
deterministic; under `step`, input is files or nothing. It is not a
guest timer interrupt: time still moves only when the guest stops
at a service point. Without the variable, nothing changes.

**Where it lands.** `tools/emulator/mmio_ring.c` (the interpreter,
`slow32-fast` and the DBT share it) and the qemu backend's own
`mmio.c`, which has `GETTIME`, `SLEEP` and timers too. Then two
tests: Kermit's timeout path on a prefix file, on all four engines;
and Asteroids on a key file, the same frames on the three C engines
(tube cases skip qemu, which has no tube: [TUBE.md](../TUBE.md) §7).

### 2. A key or a clock, from the tube

**The gap.** A terminal program waits for a key or a timeout in one
call: `term_wait_key(ms)`, or `s32_dpc_wait_on` on fd 0. A tube
program cannot. `TUBE_KEYS` never blocks, the key queue is not an
fd, and so a game that wants "the next key, or the next frame
tick" arms a short timer and drains the queue when it fires. That
is latency at best and a busy host at worst.

**The proposal.** Give the key queue a name `s32_dpc_wait_on`
accepts: a reserved descriptor, `S32_FD_TUBE_KEYS`, outside any fd
range a guest can open. Named in the list, it is readable while the
key queue is not empty, and it arrives as the same readiness entry
an fd does (kind `POLL`, `id` the reserved value, cookie `IN`),
level-triggered like the rest. Without the tube service granted it
is `NVAL`.

**Why this keeps the rules.** The readiness entry says *that* keys
are waiting, not which. The keys are still read with `TUBE_KEYS`,
4 bytes each, from the tube's own queue. Term and tube stay two
devices; the program still says which one it is waiting on; no
opcode is added, because `POLL`/`FDPOLL` already take a list.

**Where it lands.** The readiness check in `mmio_ring.c` (the tube
lives only there), `s32dpc.h` for the constant, [TUBE.md](../TUBE.md)
for the contract. A test: `S32_TUBE_KEYS` preloads two events, the guest
waits on the reserved fd and a long timer, and wakes on the keys.

### 3. When the key queue overflows

**The gap.** The queue holds 256 events and drops the oldest.
Suppose the guest has already taken a key's *down*, then falls
behind, and the matching *up* is among the events dropped. The
program now believes the key is held. A text editor never notices.
Doom turns forever.

**The proposal.** Say that it happened, in the record that is
already there. The event `reserved` byte is written 0 and ignored
on read today. When the host drops events, it sets bit 0 of
`reserved` on the oldest event it keeps: *events were lost before
this one*. A guest that cares clears its held-key state there and
reads on; a guest that does not, ignores the bit, as it ignores the
byte now. [TUBE.md](../TUBE.md) already makes gaps visible for
frames with generation numbers; this is the same idea for keys.

**Why not something larger.** A separate overflow record would be a
new kind for every guest to skip. Dropping the newest instead loses
the latest *up* just as easily, and the newest events are the ones
a program needs to catch up. Coalescing make/break pairs would hide repeats a game counts.
One bit in a byte that is already reserved is the smallest true
statement.

**Where it lands.** `tube_key_push` in `mmio_ring.c`, the event
table in [TUBE.md](../TUBE.md), a test that preloads 300 events and
checks the bit on the first one returned.
