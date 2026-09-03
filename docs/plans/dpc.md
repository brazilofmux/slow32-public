# DPCs for the emulator

Drafted 2026-09-02. A brief for whoever picks it up -- this session,
the next one, or a fresh one that has never seen the tree. It is
written to keep the scope pinned; the internals section is there so a
reader can verify the premises rather than take them.

The one-line version: **push slow32-dbt toward what an OS would need
-- timers, events, real concurrency -- without paying the bare-metal
and driver tax of Xinu, Minix or a guest under QEMU.** If the emulator
grows these capabilities, every language already hosted on it inherits
them, and an OS becomes possible later without being the price of
admission now. The shapes in mind are message passing: QNX, Minix,
Erlang.

The timer opcodes exist because a scheduler needs a tick, not because
the guest needs an IRQ. Bare metal would take a timer interrupt in
**native** code; that handler would host SLOW-32 instances. Hosted, the
emulator is that handler. The instance sees a DPC. Translated code is
never interruptible on purpose: the DBT converts large sections and
chains them until YIELD, which is why it is about five times QEMU TCG
([benchmarks.md](../benchmarks.md), [hosting.md](hosting.md)). The four
levels of composition (tasks / in-process instances / processes /
machines) live in hosting.md; this file is the tick.

## Context

A SLOW-32 instance is one thread of control and nothing else. No
interrupts, no preemption, no threads inside it, and that is one
reason it is fast: translated blocks chain to each other and never
return to the host until the guest itself hits a YIELD, a HALT, a
DEBUG, or a fault. That property stays. This work adds nothing that
runs inside an instance uninvited.

What exists today, so you don't rediscover it:

- The host regains control only at a block exit. The exit vocabulary
  is in `tools/dbt/cpu_state.h`: branch, indirect, halt, debug, yield,
  three faults, assert, block-end. The dispatcher is the
  `while (!cpu->halted)` loop in `tools/dbt/dbt.c`.
- I/O is a ring-buffer contract, `common/mmio_ring_layout.h`,
  described in `docs/MMIO_STATUS.md`. One request ring guest-to-host,
  one response ring host-to-guest, 16-byte descriptors, opcodes
  mirroring Linux syscalls. The guest TRAPs after enqueuing and YIELDs
  while waiting. The host services the rings only at YIELD, at
  deterministic points. A YIELD that finds nothing new is a spin, and
  the DBT warns after three.
- The contract already reserves a high-priority response ring "for
  timers and async signals" and says the host should drain it first,
  and it already numbers the requests this needs:
  `S32_MMIO_OP_TIMER_START` (0x32, "arm timer, host completes on HP
  ring"), `S32_MMIO_OP_TIMER_CANCEL` (0x33) and `S32_MMIO_OP_POLL`
  (0x34, "poll()/select()-style wait"), all marked future. Nothing
  behind them exists.
- The ring service is one file, `tools/emulator/mmio_ring.c`, linked
  by the reference interpreter, slow32-fast and the DBT alike. Only
  qemu carries its own copy (`target/slow32/mmio.c`). A new request
  lands once for three engines and once more for qemu.
- The ISA has no interrupt vector, no privilege modes, no trap return.
  Code is execute-only, no self-modifying code, the heap is fixed at
  link time. Any new opcode costs five engines plus the assembler and
  LLVM, and the three differential harnesses in `regression/` need
  every engine to implement the same guest-visible contract.
- One instance per host process. There is no multi-instance runtime
  and no host threading in `tools/`. Running another program is a
  child emulator process (`s32_execv`). The model permits a host that
  serves many instances from a thread pool with shared read-only code;
  that is not built.

## Design intent

Design as if this were going bare metal, but ship it hosted. The
constraint discipline without the driver tail.

The only native thing is the interrupt handler on the host. It turns
an interrupt into a DPC. **A DPC is not a function. It is a queue
entry**: a descriptor in an input queue the instance owns. Nothing is
called into, nothing is reflected into translated code, no entry
address is registered, no context is saved on the guest's behalf. The instance sees the entry when it looks. When it has nothing to do
until one arrives, it says so the same way it says everything else:
it enqueues a request -- sleep until my queue has something -- and
YIELDs. YIELD keeps its one meaning, service the rings; whether the
instance resumes is the host's decision, made from that entry. Above the host's interrupt stub, everything is
machine-independent, and the instance's own dispatch loop is ordinary
guest code.

Concurrency is between instances, never within one. An instance is a
single-threaded consumer of its queue. That is the QNX and Erlang
shape, and it is why the emulator's speed survives.

Threads and interrupts should unify into one scheduling concept on the
host rather than living as separate mechanisms. Solaris interrupt
threads and Linux threaded IRQs are the prior art; the difference here
is that both sides are under one hand, so it can be clean rather than a
retrofit.

Fixed partitions and no dynamic allocation mean resources are declared
statically, which keeps the whole thing analyzable.

The principle under all of it: threads, calls, stacks and memory are
kept separate, each with the mechanism that suits it. A thread is an
instance. A call happens inside one. A stack is the instance's own.
Memory is never shared. General-purpose OS design lets all four mix
and pushes the discipline into ABIs and conventions, where it is not
enforced; here the opinion lives in one enforced place, the ring
layout, and there is nothing else. Erlang, QNX and ARINC 653 are the
lineage. The first place the separation gets tested is a request that
needs a reply -- the second demo, after the timer -- because that is
where a call will try to come back in through the queue.

## What I want from the first session

Don't write a scheduler, and don't build multi-instance. Start with:
what is the smallest change to the emulator that demonstrates the
interrupt-to-DPC path end to end -- one timer source, one DPC, one
guest instance observing it?

Give me that first, and tell me what it forces into the emulator that
isn't there today. My own list, which you should check rather than
accept:

- **No change to the guest-visible surface at all.** YIELD keeps its
  one meaning. "Sleep until my queue has something" is a request in
  the request ring -- the reserved `POLL`, 0x34 -- and the host,
  servicing that ring at the YIELD, does not resume the instance until
  its high-priority ring is non-empty. That is a host-side decision
  made from a queue entry, which is the whole design in one place.
- **A timer is the reserved `TIMER_START`, 0x32**, armed through the
  request ring, delivered as a descriptor in the high-priority ring.
  The guest's polling read of that ring is an ordinary load of a head
  word; the ring contract's release and acquire rules already cover it.
- **Both land in `mmio_ring.c`**, so the reference interpreter,
  slow32-fast and the DBT get them together and the differential
  harnesses cover them from the first commit. qemu's copy follows.
- **The asynchronous piece is the host writing a descriptor while the
  guest runs.** The host is one thread, so the writer is a signal
  handler or a helper thread. Keep it to one producer per ring.
- **Determinism.** A wall-clock timer makes runs non-reproducible, and
  every harness here diffs engines byte for byte. The DBT doesn't
  count instructions in chained code, so a virtual-time timer isn't
  free. Decide how the demo gets a reproducible test before deciding
  how it gets a clock. This is the item to argue with first, because
  it decides whether the work can be regression-tested at all.

## The first demo, landed 2026-09-02

One timer source, one DPC, one instance observing it, on all four
engines, with no change to the ISA and no new opcode number: the two
reserved ones got implementations.

- **The DPC ring** lives at `S32_MMIO_DPC_*` in `common/mmio_ring_layout.h`:
  head and tail words at 0x0010/0x0014 and 64 entries at 0x0800, in the
  page below the request ring, which was unused. Host produces, guest
  consumes, one producer per ring. An entry is a descriptor: for a timer,
  `{opcode TIMER_START, length 0, offset id, status cookie}`.
- **`OP_TIMER_START`** arms one of `S32_MMIO_TIMER_MAX` (8) one-shot
  timers -- a fixed partition, EAGAIN when full -- with an interval and a
  guest cookie; `OP_TIMER_CANCEL` disarms. **`OP_POLL`** sleeps until the
  ring is non-empty, and answers EAGAIN when the ring is empty and nothing
  is armed, so an instance is never left asleep for something that cannot
  come.
- **Delivery is at service points only**: on the way into and out of the
  ring service (so a deadline that passes during a SLEEP is queued before
  the guest resumes), and during a POLL. One host thread, no signal
  handler, nothing written into a running guest's memory.
- **Guest side** is `runtime/include/s32dpc.h`: `s32_timer_start`,
  `s32_timer_cancel`, `s32_dpc_poll` (a load of the head word, no host
  involvement) and `s32_dpc_wait` (poll, else ask to sleep, repeat).
- **Where it landed**: `tools/emulator/mmio_ring.c` once, for the reference
  interpreter, slow32-fast and the DBT (which also mirrors the new head
  and tail in its index sync); and qemu's `target/slow32/mmio.c` once
  more. qemu's SLEEP had been a stub that returned EINTR at once; it
  sleeps now, because a timer armed before a SLEEP longer than its
  interval must have fired when the guest looks.

**What it forced, against the list above.** Less than the list feared.
No guest-visible semantic change at all. The determinism question
answered itself once the wait became a queue entry: the guest observes a
timer only by waiting for it or by looking after a sleep longer than its
interval, never by counting, so `feature-dpc-timer`'s output is the same
on every run and every engine although the clock is not. The one thing
the emulator did not have and now does is a notion of a deadline and a
place to put its consequence.

**What it did not do**, and what the next step is: the host never
writes a DPC into a running guest. A timer that expires while the
instance is computing is queued at its next YIELD, not before. Making
that asynchronous -- a signal handler or helper thread as the producer,
the guest's plain loads reading a head word another thread wrote -- is
the second demo's question, together with the request that needs a
reply.

## The second demo, landed 2026-09-02

A request that needs a reply comes back through the queue, not as a
call into the instance. `OP_POST_READ` (0x0E): the response is “the
flow was taken”; the bytes arrive as a DPC `{POST_READ, nbytes,
guest_addr, cookie}` in the **caller's own buffer**, not the MMIO
bounce (that bounce is stdio scratch; a flow owns its mailbox). If
the fd is ready, delivery is at this service point. If it would
block, the flow occupies a `POST_MAX` slot (timer-shaped, 8) and
completes at a later service point when the fd is readable, or with
0 bytes if the fd is closed. `OP_POLL` treats a pending post like an
armed timer. Same opcode; still one stack. EAGAIN only if the
partition is full. `regression/tests/feature-dpc-post-read`.

That is the anti-thread lesson in one opcode: the instance has one
set of registers and one stack; work is a flow; the fabric is the
DPC ring. A helper thread writing the ring while the guest is in
translated code is still not this demo.

## The third demo, landed 2026-09-02: wait-for-any (readiness)

The completion form (POST_READ) reads *for* you. This is the readiness
form -- tell me *when* an fd is readable and I will frame my own read --
which is what a guest with its own packet loop needs. It is the guest
`poll` opcode `hosting.md`'s order of demos calls for, and the note there
is exactly right: the host's `poll(2)` of pending POST_READ fds during
`OP_POLL` was not it; this is.

`OP_POLL` gained a payload: up to `S32_MMIO_POLL_MAX_FDS` (8) guest fds the
instance names to wait on, beside the timers and the pending posts it
already waited on. POLLIN only. A pending `POST_READ` owns that fd: no
READY DPC, so the completion's bytes are not stolen. A readable named fd
is delivered as a DPC `{kind POLL, id=fd, cookie=S32_MMIO_POLL_IN/HUP/ERR/NVAL}`,
level-triggered; a not-open fd is reported at once so a wait cannot hang
on it. Guest side is `s32_dpc_wait_on`. An inline waiter that is not the
sole consumer puts unmatched entries back with `s32_dpc_unread` (stale
timers are dropped, not unread). `feature-dpc-poll` drives it with a
delayed-pipe `stdin.sh` fixture. Consumers: kermit receive timeout and
dBase `INKEY`.

## The fourth demo, landed 2026-09-02: the cooperative scheduler

Built at direction, and it is the Level-1 reactor of `hosting.md` made
concrete rather than the OS scheduler that doc rightly forbids: **one
stack, one waiter, no preemption.** Tasks are protothreads -- guest
functions that run to an await and return, their resume line kept by a
`switch` -- so there are no extra stacks and the fibers-as-two-waiters line
in `hosting.md` is respected. `runtime/include/s32sched.h`,
`runtime/sched_mmio.c`.

`S32_AWAIT_TIMER` arms a timer, `S32_AWAIT_READ` posts a read into the
task's own buffer (POST_READ), `S32_AWAIT_READY` waits for a named fd,
`S32_YIELD` gives a compute-bound task a turn to the host via the plain
YIELD instruction. A refused post or timer fails the task
(`S32_TASK_FAIL`) instead of parking on a DPC that will never arrive.
The scheduler steps every runnable task, routes each DPC to the task
that awaits it by `(kind, id)`, and blocks on the ring only when every
task is blocked. `feature-dpc-sched`
runs a reader, a timer ticker, and a yielding worker together; the ticks all
fire while the read is in flight, proving overlap, deterministically, on all
four engines. It also gated the DBT's YIELD-spin warning on
`mmio_async_pending` so a task yielding to let I/O progress is not mistaken
for a stuck spin.

## The asynchronous producer, and why it is last

Delivering a DPC into a *running* guest -- a host thread writing the entry
and then the head word while translated code runs -- is the remaining step,
deferred for a reason that is a finding about the ISA. The ISA has no fence
and the DBT emits plain host loads: the guest reads the head word and then
the entry with no address dependency between them, so on an arm64 host a
cross-thread producer can be seen as new head, stale entry. The interpreters
could fix it with acquire loads and qemu goes through a callback, but the
DBT cannot know at translation time which loads hit the ring. So the
asynchronous producer is really the decision on the ISA's first
memory-ordering instruction, and it is argued as that -- and, per
`hosting.md`, it is the same decision as the level-2 thread pool. Neither
should land without the other.

## Multi-instance, and the wager

The destination -- many instances, shared read-only code, a thread pool,
and the same message shape reaching across processes and machines -- and the
reason the discipline is worth keeping, live in
[hosting.md](hosting.md): the four levels, what is already law, what
"build an OS, then don't" forbids, and the wager that a cut which is a
message everywhere can be *moved* rather than committed to. This file is the
mechanics and the demo log; that file is the strategy.

## What is deliberately out

- Preemption of any kind inside an instance, including a pending-event
  poll at block boundaries. It was proposed and declined: it is the
  hot path, and the model doesn't need it.
- A DPC as a callback. See above.
- A second meaning for YIELD. It was proposed and declined: the wait
  is a queue entry too.
- New opcodes, until a demo shows one is unavoidable.
- Multi-instance, thread pools, shared code pages -- the second step, laid
  out in [hosting.md](hosting.md). Deliberately out until an app asks.
