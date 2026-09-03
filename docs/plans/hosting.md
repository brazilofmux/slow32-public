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
preemption story. It is why slow32-dbt is about five times QEMU
TCG on `benchmark_core` (5.1× on the 2026-02-05 Ryzen measurement
in [benchmarks.md](../benchmarks.md); later hosts move the BIPS, not
the ratio’s cause).

Cite the other number in the same breath, because a skeptic will find
it: on `validatecsv_ragel` the same pair is **3.0×**. The ratio
narrows as a workload gets closer to real — more I/O, more YIELDs,
more service points, less time inside a chained superblock. 5.1× is
the compute-bound ceiling; 3.0× is what the product does on a
103.6 MB CSV. Both are the same effect measured at two duty cycles.

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

They are a cost gradient, and the measurement below prices it. They
are also a **failure-domain gradient, and it runs the other way**:

| Level | Cost of a cut | What dies together |
|---|---|---|
| 1 task | a function call, ns | everything — one stack, one instance |
| 2 in-process instance | *unbuilt* | every instance in that process, on a host fault or an OOM kill |
| 3 process | ~2 ms, measured | nothing else (`cluster.sh` demonstrates it) |
| 4 machine | ms + RTT | nothing — but partitions arrive, so timeouts replace exit statuses |

So promoting a cut from level 3 to level 2 to delete the startup tax
**silently weakens the isolation the wager rests on**. Relocation is
free in *interface* and not in *guarantee*. That is the stronger
reason to keep the `mmio_state` split rigorous when level 2 lands —
but note that rigor cannot close the gap, only narrow it. However
clean the split, a host-side fault, an `abort()` or an OOM kill takes
the process and every instance in it. Level 2 has a shared fate at
process granularity that no amount of separation removes; level 3
does not. That residual is the thing to declare, not engineer away.
The enforcement — a role that declares the isolation it requires, and
a host that refuses a manifest asking to co-locate it — is in
[supervision.md](supervision.md).

**1. Tasks within an instance.** Still one waiter. A reactor: work
arrives as DPC / response-ring descriptors; the instance looks when
it looks; when it has nothing it enqueues `POLL` and YIELDs.
“Tasks” are guest functions in that loop, not a second register
file. A guest fiber library (several stacks, explicit switch) is
out of bounds unless those fibers never appear to the emulator as
two waiters — `dpc.md`’s “a stack is the instance’s own.”

Landed 2026-09-02 (`runtime/s32sched.h`, `sched_mmio.c`): the reactor
as a **stackless protothread** scheduler — one stack, one waiter, no
preemption, tasks are guest functions whose resume line is kept by a
`switch`, exactly the fibers-free shape this paragraph requires. It
awaits timers, posted reads, and readiness, and routes each DPC to the
task that awaits it. “Do not write a scheduler,” below, meant the OS
scheduler with a native tick, not this reactor; the reactor was always
Level 1. Overlap I/O across instances still needs level 2.

Its named cost: cooperative scheduling means a task that does not
yield starves the others in its instance, and `S32_YIELD` makes that
discipline documented but unenforced — structurally the same shape of
obligation as “remember not to share,” differing in blast radius (one
instance) and symptom (latency, not corruption). Stated in
[seam.md](seam.md) rather than left to be discovered.

Posted read (`OP_POST_READ`, 0x0E) is the second DPC demo: a reply
comes back as a queue entry, the instance never becomes a reader
thread. A would-block fd occupies a `POST_MAX` slot and completes at
a later service point; `OP_POLL` treats that pending post like an
armed timer. Wait-for-any (named fds on the same `OP_POLL`) is the
readiness path; a pending post owns its fd so READY cannot steal the
bytes. A host producer that writes the ring while the guest is in
translated code is still missing.

**2. Multiple instances, one emulator process.** The Nintendo
process table. Shared RX code, separate data and MMIO, a dead
instance must not take the others. `hose.md` said not to start
here (“one bug kills the desk”); that was the bootstrap order.
It is now the second step, after isolation is real. Measure
against level 3 before declaring it faster — and now there is a
measurement (see “The overhead, measured” below): what level 2 buys
is the deletion of level 3’s fixed per-process startup, which is the
whole of the fine-grained plateau. Worth its cost only when a
workload needs fine-grained parallelism at high core counts; coarse
work does not.

**3. Multiple emulator processes, one machine.** Today’s product.
A first desk file landed: `examples/csvbench/cluster.sh` splits a
file list across N validator instances, runs them in parallel, and
merges in file order — the host composing machines, one bug in one
worker not taking the desk. It beats the serial instance (criterion
3), and the measurement is below. Remaining gaps: `socketpair` so two
guests on one box do not pretend to be the internet; a desk file that
gives exactly one guest the terminal; `EXEC` that can complete via DPC
(spawn, parent continues, status later) so a *guest* coordinator, not
a shell, can compose them.

**4. Multiple machines.** Numeric IPv4 already leaves. `--deny net`
that actually means it is [SERVICE_NEGOTIATION.md](../SERVICE_NEGOTIATION.md)
step 3, still the sandbox. DNS, UDP, a BSD stack stay out.

## The overhead, measured

The CSV validator (`examples/validatecsv_sched.c`, the level-1 scheduler
slice) distributed across level-3 worker processes
(`examples/csvbench/`, `make`-able and re-runnable), 64 files on 18
cores, best of five:

```
             fine files (112 KB)      coarse files (448 KB)
serial          480 ms  1.00x          1886 ms  1.00x
-j 4            160 ms  3.01x  eff 75%   533 ms  3.54x  eff 88%
-j 8            111 ms  4.31x  eff 54%   314 ms  6.00x  eff 75%
-j 18           102 ms  4.69x  eff 26%   206 ms  9.15x  eff 51%
```

The finding, and it is the one the whole shape lives or dies by:
**coordination is a fixed per-worker cost** — process spawn plus emulator
init plus the merge, a couple of milliseconds a worker — so on
fine-grained work it dominates and caps the win near 4.5x no matter how
many cores, and on coarser work it amortizes and scales to 9x+ and
climbing. Granularity, work per instance, is the knob; a real
coordinator hands each worker a fat slice, never one small file, which
is why the native code batches to threads too.

**Read that finding with its topology attached.** `cluster.sh` splits
a *file list*: every worker opens its own files and writes its own
output, and nothing crosses between workers but a name on argv and an
exit status. That is embarrassingly parallel fan-out — the shape with
no messages in it — so the only coordination cost it can expose is the
fixed one. There is a second tax it cannot see: **per byte, per hop**,
the copy across a seam, which scales with data volume rather than
worker count and is amortized by nothing. It matters because the lever
above makes it *worse*: a fatter slice amortizes the spawn cost and
moves proportionally more bytes. Granularity is the knob, but there is
an optimum, not a monotone. The unmeasured axis and the benchmark that
would settle it are [seam.md](seam.md).

This is the honest boundary against threads, and it is worth stating
in the right currency. Threads look free because their *spawn* is
free; their coordination is not, it is only unbudgeted. A shared line
under contention is cache-line ping-pong and false sharing, and that
cost is variable, invisible in the source text, and worst at exactly
the scale the threads were bought for — it surfaces as “why did adding
cores make it slower,” which is a question with no line item. The tax
in the table above is the opposite kind: fixed, visible, per worker,
and on a graph, which is why granularity can be aimed at it.

That is the trade, and it is not a claim to win every case. Below the
line where work per unit buries the fixed cost, threads win outright;
this shape is not for fine-grained work on a warm cache. Above it,
isolation and no-shared-state come free on top of competitive
parallelism — and the tax that remains is one you can see, price, and
amortize rather than discover.

Two levers, both now scoreable by `bench.sh` rather than guessed:
reduce the fixed cost (a pre-forked emulator pool, smaller worker
binaries, or **level 2** — load the code once, share it, no per-process
startup), or raise the work per worker (batching, coarser chunks).
Level 2 is the one that deletes the plateau; build it when a workload
needs fine-grained parallelism at a core count the tax is eating, and
not before.

That "not before" is now contingent. Level 2 also converts a seam
crossing from two kernel copies into one host `memcpy` — no pipe, no
syscall, and the law untouched, since the host is still the mover and
guests still cannot address each other's RAM. If the per-byte tax
dominates on a pipeline, level 2 stops being a fix for fine-grained
fan-out and becomes the fix for pipelines, which is a much larger
class of program. Settle that with [seam.md](seam.md)'s benchmark
before scheduling level 2, because it changes what level 2 is *for*.

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
(`S32_MMIO_TIMER_MAX` and `S32_MMIO_POST_MAX` are the template).

## The wager

Message passing makes a chunk *safe to run*; it does not find the cut,
and auto-decomposition of general code is a graveyard. So the goal is
not a compiler that finds the cut. It is to make a wrong cut **cheap to
try and safe to be wrong about** — cheap instances, no shared state,
let-it-crash — so iteration replaces upfront genius. Erlang never chose
anyone's supervision tree; it made reshaping it cheap.

That is why the bare-metal discipline is kept although bare metal never
ships: a fixed-partition, allocation-free, execute-only, message-only
instance is disciplined enough to run bare, which is what makes it a
small, **relocatable** unit. Because the seam is a message at every
level, a cut can be *moved* — task, to co-resident instance, to process,
to remote machine — as you learn where a problem's real independence
lives, without rewriting either side. The cut becomes a placement
decision, not a one-way commitment — a decision about latency in one
direction and about isolation in the other, which is why the levels
table above prices both.

And the fit is the workload with nowhere else to go: GPUs help only code
that vectorizes; ledgers, batch, COBOL do not, but they decompose along
seams the domain already has — accounts, transactions, report streams.
The general message-passing bet is a poor fit for graphics and a natural
fit for exactly the business logic the vector machines cannot touch.

## Order of demos

Do not write a scheduler. Do not start with multi-instance.

1. ~~DPC second demo (reply-shaped request).~~ Landed: `POST_READ`,
   pending slot, `feature-dpc-post-read`. `feature-dpc-timer` stays.
2. ~~POSIX `poll` as a **guest** opcode on arbitrary fds (timer *or*
   hose), completion as DPC.~~ Landed as **wait-for-any**: `OP_POLL`
   takes guest fds to wait on and delivers a readable one as a
   `{POLL, fd, why}` DPC, beside timers and pending posts. Distinct
   from the host `poll(2)` of `POST_READ` fds, as noted.
   `s32_dpc_wait_on`; consumers are kermit's receive timeout and
   dBase's `INKEY`. `feature-dpc-poll`.
3. `socketpair` hose + a desk file.
4. `EXEC` completing via DPC.
5. Only then level 2: split `mmio_state`, N instances, shared RX.

The opcode registry is [mmio/opcode-map.md](../mmio/opcode-map.md).
`TIMER_*` / `POLL` are implemented. They are not IRQs.
