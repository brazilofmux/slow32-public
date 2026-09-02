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
  timers and async signals" and says the host should drain it first.
  Nothing behind that reservation exists.
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
address is registered, no context is saved on the guest's behalf. The
instance sees the entry when it looks, or YIELDs until there is
something to see. Above the host's interrupt stub, everything is
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

## What I want from the first session

Don't write a scheduler, and don't build multi-instance. Start with:
what is the smallest change to the emulator that demonstrates the
interrupt-to-DPC path end to end -- one timer source, one DPC, one
guest instance observing it?

Give me that first, and tell me what it forces into the emulator that
isn't there today. My own list, which you should check rather than
accept:

- **YIELD gains a second meaning.** Today it is "service the rings and
  continue." It needs "and if nothing arrived for me, block until
  something does." That is the one semantic change on the
  guest-visible surface, and it must land in the reference interpreter
  and slow32-fast as well as the DBT, or the harnesses can't cover it.
- **A timer is a host-side request**, armed through the existing
  request ring, delivered as a descriptor in the high-priority ring.
  The guest's polling read of that ring is an ordinary load of a head
  word; the ring contract's release and acquire rules already cover it.
- **The asynchronous piece is the host writing a descriptor while the
  guest runs.** The host is one thread, so the writer is a signal
  handler or a helper thread. Keep it to one producer per ring.
- **Determinism.** A wall-clock timer makes runs non-reproducible, and
  every harness here diffs engines byte for byte. The DBT doesn't
  count instructions in chained code, so a virtual-time timer isn't
  free. Decide how the demo gets a reproducible test before deciding
  how it gets a clock. This is the item to argue with first, because
  it decides whether the work can be regression-tested at all.

## What is deliberately out

- Preemption of any kind inside an instance, including a pending-event
  poll at block boundaries. It was proposed and declined: it is the
  hot path, and the model doesn't need it.
- A DPC as a callback. See above.
- New opcodes, until a demo shows one is unavoidable.
- Multi-instance, thread pools, shared code pages. Named here so the
  reader knows they are the second step and not the first.
