# The seam

Drafted 2026-09-03, from the observation that
[hosting.md](hosting.md)'s measurement validated the friendliest
possible topology and called the result "the coordination overhead."

The one-line version: **there are two taxes, not one, and the lever
that reduces the first increases the second.**

## What the fan-out benchmark cannot measure

`examples/csvbench/cluster.sh` splits a *file list* into contiguous
slices. Every worker opens its own files, reads them from disk, and
writes its own output; the shell merges. **There is no inter-worker
data flow whatsoever.** Nothing crosses a seam except a file name on
argv and an exit status.

That is embarrassingly parallel fan-out — the topology with no
messages in it — so the only coordination cost it can possibly
expose is the fixed one: process spawn, emulator init, merge. Which
is exactly what the table found, and the finding is correct. It is
just not the whole bill.

| | scales with | amortized by | deleted by |
|---|---|---|---|
| **Fixed, per worker** — spawn, init, merge. ~2 ms, measured. | worker count | fatter slices | level 2 |
| **Per byte, per hop** — a copy across a seam. Unmeasured. | data volume × pipeline depth | *nothing* | see below |

The second one is not a smaller version of the first. It scales with
the *data*, not with the *parallelism*, so the standard remedy makes
it worse: a fatter slice amortizes the spawn cost and moves
proportionally more bytes across the seam. **"Granularity is the
knob" is true but underspecified — there is an optimum, not a
monotone**, and a real coordinator will find the wrong side of it
before it finds the right one.

Order of magnitude: a pipe or socketpair between two host processes
moves roughly 1–3 GB/s with two kernel copies. A five-stage pipeline
over a gigabyte pays about five gigabytes of copying. Threads pay
zero for the same transfer — they pass a pointer. This is the one
place where "threads win" is not about fine granularity at all, and
where the no-shared-memory law leaves no escape hatch. Erlang met
exactly this wall and had to add refcounted off-heap binaries;
that is the concession the pure model could not sustain.

## The benchmark that would measure it

Same corpus, different shape. Not another fan-out.

**Topology.** Three stages over the CSV corpus, each a separate
instance, connected by hoses:

```
  stage 1  parse      →  stage 2  validate  →  stage 3  aggregate
           records                 verdicts               totals
```

Stage 1 reads files and emits parsed records. Stage 2 consumes
records, applies the validator's rules, emits verdicts. Stage 3
consumes verdicts and emits totals. Every record crosses two seams.

**The control, and this is the part that makes it a measurement
rather than a demo:** the identical three-stage decomposition run as
three *function calls inside one instance*, over the same corpus,
with the same record struct. Same work, same cuts, zero seams. The
difference between the two is the per-byte tax, isolated, with
everything else held constant.

**What to record.**

- Bytes across each seam; total bytes moved vs. total bytes of input.
- Wall time for: 1 instance / 3 calls (control), 3 instances / 2
  hoses, and the same 3 stages fanned out ×N.
- Throughput as a function of **record size** — this is the axis
  that matters, because the tax is per byte and the fixed cost is
  not. Sweep small records to large ones and find where the curves
  cross.
- Time in `write`/`read` on the seam vs. time in the stage's own
  work. The ratio is the number.

**The prediction to falsify.** At small records the fixed cost
dominates and the pipeline looks like the fan-out table. At large
records the copy cost dominates and the three-instance version
loses to the one-instance control, possibly badly. If that crossover
does not appear, the tax is smaller than argued here and the doc is
wrong — which is a fine outcome, and better learned from a
measurement than from an argument.

**What it is not.** Not a throughput record, not a comparison
against native code, not a claim about CSV. It is one number: what a
seam costs per byte, and where that overtakes what a seam saves.

## Room to play: where the tax can be attacked

This is the part the transputer did not have. Inmos owned the
silicon and the language and nothing in between, and when the copy
cost hurt there was one place to fix it. Here the stack is
controlled from COBOL and C++ down to the virtual silicon, so the
same tax can be attacked at whichever layer is cheapest — and the
layers are listed in increasing order of how much they disturb:

1. **Don't cross the seam.** Stage fusion at the language or
   coordinator level: two stages that always run together are one
   instance and one function call. The cheapest fix for the copy is
   not making it. This is the level-1 reactor's job and it is
   already built.
2. **Cross it less often.** Batch records into blocks; amortize the
   ring round trip over many records. Runtime and libc work, no
   contract change.
3. **Make the host's copy cheaper.** Guests keep seeing `read` and
   `write`; the host uses `splice(2)`/`vmsplice(2)` between two
   guest processes and moves pages instead of bytes. Guests never
   learn. This is precisely "the host composes machines" — the
   optimization lives where the law already says the composition
   lives.
4. **Level 2 deletes the kernel entirely.** Two instances in one
   emulator process means a guest-to-guest transfer is one `memcpy`
   inside the host, with no pipe, no syscall, no scheduler round
   trip. **The law survives untouched**: guests still cannot address
   each other's memory, the host still moves the bytes, nothing is
   shared. What disappears is the kernel, not the isolation.
5. **Transfer ownership instead of copying.** A hose descriptor that
   hands the receiving instance a buffer the host has already filled
   — `POST_READ` set the precedent when it began writing the
   caller's own memory rather than the bounce. This is the deepest
   change and the one most likely to smuggle shared memory in
   through the back door; it needs the argument written before the
   code.

**And this reframes level 2.** [hosting.md](hosting.md) justifies
it as deleting the fixed per-process startup — the fine-grained
plateau. That is real, but it may be the smaller half. Level 2 also
converts every seam crossing from *two kernel copies* into *one host
`memcpy`*, which is the only structural attack on the per-byte tax
that does not touch the contract. If the pipeline benchmark shows
the copy cost dominating, level 2's case stops being "worth it for
fine-grained parallelism at high core counts" and becomes "the fix
for pipelines," which is a much larger class of program.

That is a claim the benchmark can settle, and it should be settled
before level 2 is scheduled, because it changes what level 2 is
*for* — and therefore what it has to be good at.

## The other unnamed cost: level 1's yield obligation

While being honest about taxes. Level 1 is cooperative, by law: no
preemption inside an instance, ever. Cooperative scheduling's
failure mode is a task that does not yield starving every other task
in its instance — the exact problem preemption was invented to
solve. `S32_YIELD` exists for compute-bound tasks, which means the
discipline is **documented but unenforced**: every task author must
remember.

That is structurally the same kind of obligation this design
criticizes threads for ("you must remember not to share"). The
difference is real and worth stating rather than eliding:

| | forgetting to lock | forgetting to yield |
|---|---|---|
| blast radius | global | one instance |
| symptom | silent corruption | latency |
| discovery | under load, non-reproducibly | immediately, in one place |

Latency in one instance is a far better failure than corruption
anywhere, and the blast radius is bounded by the same isolation the
rest of the design buys. But it is a difference of degree, not of
kind, and a COBOL sort inside a task will block that instance's I/O
with nothing in the system to stop it. Name it as a cost of level 1
rather than letting someone discover it.

## What this does not change

- No guest-to-guest shared memory. Every attack listed above keeps
  the host as the mover; none of them lets one guest address
  another's RAM.
- No preemption inside an instance. The yield obligation is a named
  cost, not an argument for a timer that interrupts guest code.
- The fan-out measurement stands. It is correct for its topology and
  `cluster.sh` remains the level-3 desk file. What is added is the
  second axis, not a retraction of the first.
