# Hosting more than one workload

Drafted 2026-09-02, after Fortran 77, COBOL 85 and stage08 C99 had
reached as far as they needed to for a while. The languages stay;
the next push is the machine’s ability to *host* them together.

Two vectors, one constraint.

**Four levels of composition**

1. Tasks within an instance — still no threads, still no interrupts
2. Multiple instances per emulator process
3. Multiple emulator processes on the same machine
4. Multiple machines

**The OS you could build and will not.** Design as if a native
scheduler, driven by a timer interrupt in *native* code, hosted
SLOW-32 instances at whatever ring the silicon used. Then do not
write that OS. Nintendo and the PS4 are the shape: the application
thinks it owns the machine; the kernel and the system services are
someone else’s native code; IPC is a hose. A Switch game does not
ship an SD driver.

## Why there is no guest timer interrupt

`opcode-map.md` once listed `TIMER_START` as “planned” because the
obvious next step looked like Xinu: you cannot schedule without a
tick, a tick is an interrupt, therefore the ISA needs one.

That was the wrong machine.

Bare metal **would** have a timer interrupt. It would land in
native code. That handler would implement threads (or whatever the
host scheduler is) and those threads would *host* SLOW-32
instances. The instance would never take the IRQ. Ring 0 vs ring 3
is a native question. The guest-visible surface is still: one
thread of control, YIELD services the rings, a timer that fired is
a **DPC** (a queue entry) waiting when the instance next looks.

Hosted, the emulator *is* that native code. `TIMER_START` /
`TIMER_CANCEL` / `POLL` and the DPC ring ([dpc.md](dpc.md), landed
2026-09-02) are the tick, without a vector, without `ERET`, without
a guest kernel.

## Why the DBT is allowed to be this fast

QEMU TCG is a general emulator: translated code must remain
interruptible, so blocks stay small and the dispatcher is always
nearby. SLOW-32 DBT does the opposite on purpose. It converts
**larger** sections (superblocks, chaining, register cache) and
does not apologize that it handles interrupts badly, because it
does not handle them in translated code at all.

Control returns to the host at YIELD, HALT, DEBUG, or a fault —
the vocabulary in `tools/dbt/cpu_state.h`. That is the whole
preemption story. It is why, on `benchmark_core`, slow32-dbt is
about five times QEMU TCG (5.1× on the 2026-02-05 Ryzen
measurement in [benchmarks.md](../benchmarks.md); later hosts move
the BIPS, not the ratio’s cause).

Peeking at a DPC head on every translated block, or shrinking
superblocks “just in case,” would spend that ratio to buy an OS
we are not writing. Declined in [dpc.md](dpc.md)
(“preemption of any kind inside an instance”).

## What is already law

Do not reopen lightly. Full sentences live in the cited docs.

| | |
|---|---|
| The emulator is the OS | [1987-desk.md](1987-desk.md) |
| A serial port, TCP, and a `socketpair` are the same MMIO shape. Guests do not share RAM. The host composes machines. | [hose.md](hose.md) |
| “No I/O, no shared memory, no threads. Just computation and messages.” Parallelism is instances. | [host-interface-design.md](../host-interface-design.md) |
| One instance = one thread of control. A host interrupt becomes a queue entry, never a vector. Concurrency is **between** instances. | [dpc.md](dpc.md) |

Level 3 is partly landed: `EXEC` (COMMAND.COM, BBS doors), IPv4 TCP
hose (`net/`, opcodes `0x40–0x48`). Level 1’s timer→DPC path is
landed; the request-that-needs-a-reply is not. Level 2 is named in
`dpc.md` as the second step and is not built (`mmio_state` is still
global — [AUDIT-2026-08.md](../AUDIT-2026-08.md)). Level 4 is TCP
that has left the box; policy and a desk file whose far end is an
IP are the missing work, not a new protocol.

[thread-service-routines.md](../thread-service-routines.md) and the
TSR/EVT notes in [file-formats.md](../file-formats.md) are
archaeology: shared-memory extra CPUs inside one instance. DPC won.

## The four levels

**1. Tasks within an instance.** Still one waiter. A reactor: work
arrives as DPC / response-ring descriptors; the instance looks when
it looks; when it has nothing it enqueues `POLL` and YIELDs.
“Tasks” are guest functions in that loop, not a second register
file. A guest fiber library (several stacks, explicit switch) is
out of bounds unless those fibers never appear to the emulator as
two waiters — `dpc.md`’s “a stack is the instance’s own.” Overlap
I/O by running two instances.

Posted read (`OP_POST_READ`, 0x0E) is the second DPC demo: a reply
comes back as a queue entry, the instance never becomes a reader
thread. POSIX `poll` on guest fds (timer *or* hose) and a host
producer that writes the ring while the guest is in translated code
are still missing.

**2. Multiple instances, one emulator process.** The Nintendo
process table. Shared RX code, separate data and MMIO, a dead
instance must not take the others. `hose.md` said not to start
here (“one bug kills the desk”); that was the bootstrap order.
It is now the second step, after isolation is real. Measure
against level 3 before declaring it faster.

**3. Multiple emulator processes, one machine.** Today’s product.
Gaps: `socketpair` so two guests on one box do not pretend to be
the internet; a desk file that starts N emulators, pre-wires
hoses, gives exactly one the terminal; `EXEC` that can complete
via DPC (spawn, parent continues, status later).

**4. Multiple machines.** Numeric IPv4 already leaves. `--deny net`
that actually means it is [SERVICE_NEGOTIATION.md](../SERVICE_NEGOTIATION.md)
step 3, still the sandbox. DNS, UDP, a BSD stack stay out.

## What “could build an OS, then don’t” forbids

- No interrupt vector, privilege rings, or `ERET` in the ISA.
- No guest kernel as a required runtime. Apps keep linking
  `libc_mmio`.
- No guest-to-guest shared RAM until a program exists that a hose
  cannot express.
- No intra-instance preemption, including a DPC peek every block.
- Do not unify TSR and DPC.

What it requires: every new capability is a **service**, a **hose**,
or another **instance**. Host-native code may take interrupts and
may only **enqueue**. Fixed partitions where we can
(`S32_MMIO_TIMER_MAX` is the template).

## Order of demos

Do not write a scheduler. Do not start with multi-instance.

1. ~~DPC second demo (reply-shaped request).~~ Landed: `POST_READ`,
   `feature-dpc-post-read`. `feature-dpc-timer` stays.
2. POSIX `poll` on guest fds, completion as DPC or response ring.
3. `socketpair` hose + a desk file.
4. `EXEC` completing via DPC.
5. Only then level 2: split `mmio_state`, N instances, shared RX.

The opcode registry is [mmio/opcode-map.md](../mmio/opcode-map.md).
`TIMER_*` / `POLL` are implemented. They are not IRQs.
