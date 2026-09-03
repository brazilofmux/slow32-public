# Supervision

Drafted 2026-09-03. The missing half of [hosting.md](hosting.md)'s
wager. That doc bets on cheap instances and let-it-crash: "make a
wrong cut cheap to try and **safe to be wrong about**." Let-it-crash
is not a property of the thing that crashes. It is a property of the
thing that notices. Today the only thing that notices is a shell
script, and a shell script cannot be part of the system it supervises.

The one-line version: **`EXEC` completing as a DPC is not async spawn,
it is the precondition for a supervisor that is itself a guest.**
That is why it is worth more than its position (item 4) on
hosting.md's demo list suggests.

## The law it appears to break, and does not

[hose.md](hose.md) says, under "what not to start with":

> A guest that boots the cluster. A guest that can spawn arbitrary
> `.s32x` is a confused deputy. The host already is the OS.

That law is right and stays. The deputy problem is real: a guest
granted `exec` picks the image *and* inherits the authority to run
it, so it can be talked into running something its grantor never
intended. `EXEC` (0x10) already bends this for COMMAND.COM and BBS
doors, and the bend is tolerable only because the child is a fresh
emulator whose capabilities the host, not the parent, decides.

The resolution is to make that explicit and to remove the image from
the guest's hands entirely:

> **A supervisor instantiates roles, not images.** The desk file
> declares a role — a name, an image, a capability set, a placement,
> a ceiling on instances. The guest says `spawn("validator", slice)`.
> It chooses *when* and *how many*; it never chooses *what* or *with
> what authority*.

That is a bounded factory, not a deputy. The host is still the OS: it
owns the namespace, and a role the manifest does not declare cannot be
spawned at any price. It also collapses two items on hose.md's
bootstrap list — the desk file (2) and `exec` as a service (3/4) —
into one feature, which is the right shape, because a spawn primitive
without a manifest has nowhere to get the child's capabilities from
except the parent, and that is exactly the deputy.

## What a supervisor actually needs

Erlang is the lineage dpc.md already claims. Its inventory, and where
each piece lands here:

| Erlang | Here | Whose job |
|---|---|---|
| Cheap processes | An instance. ~2 ms at level 3 ([hosting.md](hosting.md), measured) | host |
| Total isolation | Stronger than Erlang's: separate address spaces, W^X, no shared heap | ISA |
| Mailbox, selective receive | The DPC ring; `s32sched` already routes by `(kind, id)` | landed |
| **Monitor** (notification) | A completion DPC. New. | guest sees it |
| **Link** (propagation) | Parent process dies → host reaps its children | **host**, not guest |
| **Kill** | An opcode on a handle | guest |
| Supervision strategy, restart intensity | Ordinary guest code | guest |

Two of those splits are the design, and both follow dpc.md's rule that
host-native code *may only enqueue*:

- **Noticing is a queue entry.** A child that ends becomes a DPC. The
  parent is never called into, never gets a signal, never has a
  handler registered on its behalf.
- **Reaping is the host's.** A parent whose emulator process dies
  cannot run code to clean up — that is precisely the moment it has no
  thread of control. So downward propagation is a host guarantee, not
  a guest obligation. The guest cannot forget to do it, because the
  guest never does it.

## The shape

Three additions, no new wait machinery.

| | | |
|---|---|---|
| `SPAWN` | `0x11` | role name + argv in `DATA_BUFFER`; `status` = guest cookie. Returns a **handle** `0 .. CHILD_MAX-1`, or `EAGAIN` (partition full) / `EPERM` (no such role, or ceiling reached). Optionally pre-wires a hose (below). |
| `CHILD_KILL` | `0x12` | `status` = handle. The child ends; its completion DPC still arrives, with reason `KILLED`. |
| `SOCKETPAIR` | `0x49` | Two connected guest fds, or one end handed to a spawned child. hose.md item 3. `0x49–0x4F` are free in the IPC range. |

Completion is a DPC in the existing ring:

```
{ opcode SPAWN(0x11), length reason, offset handle, status cookie }
```

reaching the guest as `s32_dpc_t { kind = S32_DPC_CHILD, length =
reason, id = handle, cookie }`. The handle frees when the DPC is
harvested, exactly as a timer id frees when it fires.

`S32_MMIO_CHILD_MAX` is a fixed partition, the same template as
`TIMER_MAX` / `POST_MAX` / `POLL_MAX_FDS` (all 8).

**Open number.** 8 is too few for the measured bench, which ran `-j
18`. The Erlang answer is a *tree* — supervisors spawn supervisors,
8 wide by 2 deep is 64 — and the tree is the shape we want anyway.
But 8 forces a tree on a workload that wanted a flat list of 18, and
that is the wrong reason to be pushed into a design. 16 is the
proposal; the argument for 8 is uniformity and for 32 is never
thinking about it again. Pick before writing the header, not after.

### The punchline: the reactor is already the supervisor

`s32sched` dispatches DPCs to the task awaiting `(kind, id)`, and
`OP_POLL` already waits across timers, pending posts, and named fds.
Add one DPC kind and one fd source and a supervisor is an ordinary
protothread awaiting a mixed set of {child exits, child chatter,
deadlines}:

```c
#define S32_AWAIT_CHILD(t, h, preason)                                \
    do {                                                              \
        (t)->wk = S32_DPC_CHILD;                                      \
        (t)->wid = (unsigned)(h);                                     \
        (t)->line = __LINE__; return S32_TASK_WAIT; case __LINE__: ;  \
        *(preason) = (int)(t)->done.length;                           \
    } while (0)
```

That is the whole guest-side addition. **Supervision needs no new
wait primitive** — the wait-for-any that landed 2026-09-02 was, it
turns out, the hard part. A supervisor that spawns a child, wires a
socketpair, awaits either the child's chatter or a 5-second deadline,
and reaps on exit, is one task function with three existing macros
and one new one.

## Exit status is not enough; carry a reason

A `.s32x` can end by `EXIT(code)`, by `HALT`, by one of the DBT's
three faults, by an assert, by exhausting a cycle limit, by
`CHILD_KILL`, or by never having loaded at all. An exit *code*
flattens all of that into a byte the child chose — and a child that
faulted never chose anything.

So `length` carries a **reason class** beside the code: normal exit
(with the code), faulted, killed, never-started. The exact set should
be lifted from the exit vocabulary in `tools/dbt/cpu_state.h` rather
than invented here, since that is what the engines already agree on.

This matters because a supervisor's central decision — restart or
give up — turns on *why*, not on *what*. Erlang's `normal` vs
abnormal exit is the same distinction, and getting it wrong is how
you build a thing that cheerfully restarts a program that cannot
start.

## Argue with this first: determinism

dpc.md put the determinism question first and was right to: three
harnesses in `regression/` diff engines byte for byte, and a feature
that cannot be tested that way is a feature that rots.

Child completion order is wall-clock. Two children that both finish
"about now" can land in one service point or two, in either order, on
either engine. That is not fixable by sorting.

The proposal, in two parts:

1. **The contract does not specify delivery order.** Say so out loud.
   A supervisor does not need to know who finished first — that is a
   race, and making it unobservable removes the bug class, the same
   way refusing shared memory does. Within one service point, deliver
   by ascending handle: free, and it removes one axis of noise.
2. **Tests are written order-insensitively.** Collect statuses into
   an array indexed by handle; print at the end. This is not a new
   discipline — it is exactly what `examples/csvbench/cluster.sh`
   already does when it merges output in *slice* order rather than
   completion order, and that script is the working precedent.

The residual is honest: a test cannot assert *when* a child's DPC
arrives, only that it does and what it says. `feature-dpc-child`
should therefore spawn children whose outputs are order-independent,
and a single-child test should carry the exact-sequence assertions.

## Placement changes the guarantee, not just the cost

[hosting.md](hosting.md)'s four levels are a cost gradient: L1 is a
function call, L3 is ~2 ms measured, L2 is unbuilt and is the missing
middle. But they are also a **failure-domain gradient, and it runs
the other way**:

| Level | Cost of a cut | What dies together |
|---|---|---|
| 1 task | ns | everything — one stack, one instance |
| 2 in-process instance | *unbuilt* | all instances in that process, on a host fault or OOM kill |
| 3 process | ~2 ms | nothing else (demonstrated by `cluster.sh`) |
| 4 machine | ms + RTT | nothing — but now partitions, so timeouts replace exit statuses |

So promoting a cut from L3 to L2 to delete the startup tax **silently
weakens the isolation the wager rests on**. hosting.md currently
treats relocation as free; it is free in *interface* and not in
*guarantee*.

The rule that keeps it safe, and it belongs in the manifest because
the manifest is the only place the host can enforce it:

> A role declares the isolation it requires. A role marked `isolate`
> is never co-located, and the host refuses a manifest that asks. A
> supervisor may move a cut for speed without being able to move it
> out of its own safety.

That turns "the cut is a placement decision" from a slogan into
something checkable at manifest-load time, before anything runs.

## Restart policy is guest code

The manifest declares roles, capabilities, placement and ceilings.
It does **not** declare restart strategy. That lives in the guest,
for the reason hosting.md gives: restart policy is the thing you will
get wrong first and want to change, and the wager is that iteration
replaces upfront genius. Erlang puts supervisor specs in Erlang for
the same reason. It also keeps the host manifest small, which hose.md
already insists on.

One thing the guest supervisor must not omit, and the reason to say
it here: **a restart intensity limit is mandatory, not optional.** At
a measured ~2 ms per instance, a program that faults immediately
restarts about 500 times a second. Erlang's answer — at most R
restarts in T seconds, or the supervisor itself gives up — is the
right one, and the period window is `S32_AWAIT_TIMER`, which already
exists.

## What this forbids

- No guest-chosen image paths. Roles only.
- No guest-chosen capabilities for a child. The manifest decides;
  a child is never handed authority the parent holds.
- No signals, no process groups, no `SIGCHLD`. `CHILD_KILL` takes a
  handle, and the notification is a queue entry like everything else.
- No supervisor in the host. The manifest declares; it does not
  restart. Restarting is writing the OS.
- No blocking `waitpid`. The whole point is that the parent stays a
  reactor; `EXEC` (0x10) keeps its synchronous meaning for
  COMMAND.COM and doors, and `SPAWN` is the async one. Two opcodes,
  not a flag.
- No guest-to-guest shared memory, still. A supervisor is a stronger
  temptation than most; the answer is still a hose.

## Order of work

1. `SOCKETPAIR` (0x49) + a desk file. hose.md item 3. Independently
   useful — two pre-wired guests with no supervisor at all — and
   simpler. Reserve `SPAWN`'s wire-me-a-pair field now so spawn does
   not need a v2.
2. The manifest: roles, capabilities, placement, ceilings, `isolate`.
   Host-side only; nothing guest-visible yet.
3. `SPAWN` (0x11) + the child DPC + `CHILD_MAX`. Lands once in
   `tools/emulator/mmio_ring.c` for three engines, once more in
   qemu's `target/slow32/mmio.c`, per dpc.md.
4. `S32_AWAIT_CHILD` in `s32sched.h`; `feature-dpc-child`.
5. `CHILD_KILL` (0x12), which is only meaningful once a supervisor
   has a deadline to enforce.
6. A guest coordinator that replaces `cluster.sh` — the criterion is
   that it beats the shell on the *fine-grained* column of
   hosting.md's table, since that is where the shell's per-worker
   spawn cost is the whole story.

Level 2 is still after all of this. It is a performance fix for a
plateau that batching already dodges — coarse work scales to 9x+
today — whereas supervision is a capability nothing substitutes for.
