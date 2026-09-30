# DBT hooks: native routines the guest opts into

Status: design, 2026-09-29. Nothing below is built yet except the
profiler that chose the candidates (`slow32 -p`, 8c829339).

## What exists today

slow32-dbt already replaces some guest functions with host code. It
finds them by name in the `.s32x` symbol table and translates each one
as a stub that calls the host function and returns to r31. There are two
kinds:

- the byte intrinsics (memcpy, memset, memmove, strlen, memswap, memcmp),
  each with its own emitter per host (translate.c, translate_a64.c);
- the libm intercepts (`math_intercepts[]` in dbt.c): name, host
  function, and one of eleven register signatures (SIG_F64_F64, ...),
  with an emitter per signature per host.

Both kinds hook **by plain name** and **must be total**. Whatever
`memcpy` means in the guest binary, the DBT assumes it is the C
function, and the stub cannot hand a case back to the guest. That is
safe for the C library, whose semantics are fixed by the standard. It is
not safe for libcob, which changes every week.

## The requirements

1. **Optional.** slow32, slow32-fast and qemu never hook, and the DBT
   may decline, per routine or per call. So the guest routine is the
   reference and must always work. A hook is an accelerator, never a
   dependency.
2. **Exact.** A hook's effect on registers and memory is the guest
   routine's, byte for byte, for every input it accepts.
3. **Durable across drift.** A guest binary and a DBT are built and
   shipped separately (the `slow32:emulator` and `slow32:cobol` images).
   An old DBT meeting a new libcob, or the reverse, must never apply a
   hook whose contract has changed. It must emulate instead.
4. **Cheap to add.** One C function per hook. No per-host assembly per
   hook: today, a new intercept signature costs two hand emitters.

## The design

### The guest declares what may be hooked

A hookable routine gets a second symbol at its entry:

    __s32hk_<name>_<tag>

where `<tag>` is 8 hex digits of a hash of the routine's contract. The
DBT hooks only symbols of that shape whose `<name>_<tag>` it knows. It
never hooks by plain name. An unknown tag, an unknown name, or no tag
symbol at all means the guest code runs, translated as usual.

**The tag is a content hash, not a version number.** Hookable libcob
code lives in its own source file (the *kernel file*, below), and the
build hashes it. Both builds hash the same file: libcob's for the symbol
name, and the DBT's for its table. So any edit to a kernel changes the
tag, and a DBT built before the edit stops hooking that kernel on its
own. The cost is a spurious mismatch when only a comment changed, which
means a rebuild, never a wrong answer. A hand-bumped version is one
forgotten bump away from a silently wrong hook. That is the failure a
hook must never have, so the tag is not left to memory.

### Declining: the entry is a one-instruction thunk

The hookable symbol is a jump to the implementation:

    cob_edit_apply:
    __s32hk_cob_edit_apply_1f3c9a07:
        jal  r0, cob_edit_apply_impl

Other engines execute the jump: one instruction per call, the price of
optionality. The DBT translates the thunk as a hook stub:

- it calls the host function;
- on **done**, it returns to r31, as the intrinsic stubs do;
- on **decline**, it branches to the jump's target, an ordinary chained
  exit into the translated guest implementation.

Declining makes partial hooks possible. A hook takes the common cases
(DISPLAY and COMP-3, say) and declines the rest (national, a PICTURE
with P) instead of reimplementing all of it. It also settles faults: a
hook checks every guest range it will touch and **declines if any is
out of bounds, in MMIO, or a write below the rodata limit**. The guest
routine then runs and faults exactly as the reference does. A hook
never has to emulate a fault.

The other way to decline, re-translating the entry under a side key,
needs no thunk but touches the block cache, chaining and flushes. The
thunk keeps all of that in the guest, visible in a disassembly: you can
read off `slow32dis` what a binary lets the DBT replace.

### One stub, one host signature

Every hook has the same host signature:

    int hook(dbt_cpu_state_t *cpu, uint8_t *mem);   /* HK_DONE or HK_DECLINE */

It reads its arguments from `cpu->regs[3..10]` (and the stack, through
`mem`, for more), writes `regs[1..2]`, and reaches guest memory only
through

    void *hk_ptr(cpu, mem, guest_addr, len, int write);   /* NULL: decline */

which applies the checks the translated loads and stores apply. So there
is one emitter per host (x86-64, AArch64) for all hooks, written once:

1. spill the register cache;
2. call `hook(cpu, mem)`;
3. on done, jump to r31;
4. on decline, exit to the thunk's target.

A hook is plain C in `tools/dbt/hooks.c` plus a table row:

    { "cob_edit_apply", COB_HK_TAG_EDIT, hk_cob_edit_apply }

`-I` turns hooks off with the intrinsics, and `-H` turns off hooks
alone. `S32_HOOKS=name,name` enables only the listed hooks, for
bisection. `-s` counts calls and declines per hook: a hook that mostly
declines is not paying for itself.

### Shared source: the hook *is* the reference, compiled twice

The strongest guard against drift is not to have two implementations.
The hot libcob routines get factored into **kernels**: pure C
functions, each in the kernel file (`libcob/kern.c`), under these rules:

- they touch no globals. Anything like DECIMAL-POINT IS COMMA or the
  currency sign comes in as an argument;
- they call nothing outside the kernel file (memcpy/memset excepted);
- they read guest structures through **guest-layout mirrors**. A pointer
  field is a `uint32_t` in the mirror, and code follows it through
  `KP(p)`. In the guest that is a cast. In the DBT it is `hk_ptr`, and a
  NULL makes the hook decline.

libcob compiles `kern.c` for SLOW-32 and calls the kernels through
their thunks. The DBT compiles the *same file* for the host, with
`KP()` bound to `hk_ptr`. Each hook is a few lines of glue:

1. unpack the registers;
2. check the ranges;
3. call the kernel;
4. store r1.

The kernel file's hash is the tag. So the DBT's native code for a hook
is the guest's code, from the same bytes of source.

What is left to go wrong is two compilers disagreeing over the same C:
the slow32 backend and the host compiler, or stage08 when libcob is
built by the self-hosted fallback. That is exactly what a differential
catches.

### Verification

- **Kernel differential**, the analog of `tests/wide_test.c`: a guest
  program drives every kernel over random and edge inputs and prints the
  results. It runs under slow32-fast (no hooks) and slow32-dbt (hooks),
  and the outputs must be byte-identical. A mutation check confirms that
  a deliberately broken hook is caught, per the differential-vacuity rule.
- **Whole-program:** the cobol harness, CCVS-85, majesty and the Open
  Systems papers under the DBT with hooks on and off, byte-identical.
  run-differential.sh covers the builtins.
- **Stats in the gate:** a hook that the harness never calls is not
  tested. The kernel differential must call every hook (the `-s`
  counters say so).
- shadow_interp (`--paranoid`) treats a hook stub as it treats the
  intrinsic blocks: it skips verification at that PC.

## Candidates, from the profile

Instructions per kernel under the reference interpreter, after
8c829339. The DBT already runs memcpy/memmove/memcmp natively, so those
rows are excluded. Under the DBT, where 23G instructions take 3.3 s,
the ranking holds for the rest.

| family | where it is hot | share |
|---|---|---|
| 64-bit division builtins (`__udivdi3` and the other three) | karith, kedit, ksort; every C program | 4-15% |
| numeric decode/encode: `cob_get_num`, `cob_put_num_x`, `mag_to_digits`, `udiv_pow10` | every kernel | 20-45% |
| editing: `cob_edit_apply`, `cob_deedit` | kedit | 41% |
| INSPECT, STRING, UNSTRING: `cob_inspect_run`, `cob_str_src`, `cob_unstr_into` | kstring | 44% |
| comparison: `cob_cmp`, `cmp_bytes` | ksearch | 32% |

Not candidates: `bt_pin` (kidx 16%) is a cache lookup, an algorithm
problem, not a constant factor. The wide stack is rare by design.

## Order of work

1. **The mechanism, proved on the builtins.** Add the thunk and tag
   convention to libs32's four 64-bit division routines. Their tag is
   the hash of their source file, the same rule, though the file never
   changes. A zero divisor declines, so the guest's own zero semantics
   stand. Then the generic stub on x86-64 and AArch64, `-H`,
   `S32_HOOKS`, the `-s` counters, and the shadow skip. Measure karith.
2. **The kernel file and the first libcob kernel:** numeric decode and
   encode for DISPLAY, COMP-3 and BINARY, declining the rest. Then the
   kernel differential and its mutation check.
3. **Editing, then INSPECT/STRING/UNSTRING, then comparison,** each
   measured under the DBT, each kept only if it pays.

## Anticipation: what is committed, and what is not

Every piece of the interface is either in the guest binary or in the
DBT, so nothing here is as permanent as an instruction. Still, some
choices bind what is already built and shipped:

- **The `__s32hk_<name>_<tag>` shape and the thunk.** A shipped binary
  carries them forever. Changing the shape later leaves old binaries
  unhooked (they still run), and the DBT can keep matching old shapes.
- **Hook = guest routine, exactly.** This is the rule that keeps every
  engine interchangeable, the way the differential harnesses assume.
- **The host signature is private to the DBT** and can change freely.
  So can everything about how a hook is emitted.
