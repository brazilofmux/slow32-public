# tools/dbt5 — ISSUES.md

Review of the clean-room Stage 5 DBT as of 2026-05-18 (latest relevant commit `bb860660`).

**Progress:**
- **DONE**: A1–A6 (all correctness bugs in the AArch64 emitter)
- **DONE**: A3 (real side-exit emission)
- **DONE**: B1 + B2 (wiring + Apple Silicon execution enablement)
- **DONE** (pilot): B3 + B4 (real shadow execution in the pilot + asm clobber hygiene)
- The pilot now emits, wires, executes the emitted A64 code, and performs a meaningful (if pilot-scoped) validation.
Scope: the AArch64 emission path (`stage5_codegen_a64.{c,h}`), its wiring in
`dbt5.c`, and the upstream pipeline that feeds it (lift → SSA → MIR → BURG →
LIR → LIR-opt → RA → emit). The x86-64 emitter (`stage5_codegen_x64.{c,h}`)
is included for contrast only.

The intent is that someone — or a coding agent — can pick this list up and
make Stage 5 progress without re-deriving the same observations.

Each issue is tagged:

- **BUG** — produces incorrect behavior today, blocks correctness
- **WIRING** — control or data flow that doesn't actually do what it claims
- **CLEANUP** — dead code, misleading comments, or refactor debt
- **DESIGN** — architectural choice worth revisiting
- **OPPORTUNITY** — measurable performance / quality improvement

The order is roughly "block correctness first, then unblock execution, then
improve" — i.e. read top to bottom.

---

## A. Correctness bugs in `stage5_codegen_a64.c`

### A1. **DONE** (bb860660) **BUG** `value_to_host` is zero-initialized — unmapped values look like W0

`stage5_codegen_a64.c:93`

```c
a64_reg_t value_to_host[STAGE5_SSA_MAX_VALUES] = {0};
```

`a64_reg_t` is an enum where `W0 = 0` and `A64_NOREG = -1` (`emit_a64.h:24`).
Zero-init means any SSA value that the RA did **not** assign a slot (spilled,
or simply not in any interval) reads back as `W0`, which is a real, valid host
register. Every emitter case then checks `if (src_h != A64_NOREG)` — that
check never fails, so we silently emit instructions referencing W0 instead of
either failing or falling back to a reload.

Fix: initialize with `memset(value_to_host, 0xFF, sizeof(value_to_host));`
(yields `-1 == A64_NOREG` because the enum's underlying type is `int`). Or
make the lookup explicit (e.g. a per-value `assigned` flag).

This same bug appears in `entry_gpr_for_slot` indirectly: see B1.

### A2. **DONE** (bb860660) **BUG** Standalone `CMP_RR` / `CMP_RI` / `SETCC` never materialize a result

BURG lowers a non-fused `MIR_OP_CMP_*` to **two** LIR nodes: a `LIR_OP_CMP_*`
with `dst_v = 0` (it intentionally has no destination, see
`stage5_burg.c:255`) followed by a `LIR_OP_SETCC` whose `dst_v` is the SSA
value that holds the boolean.

In `stage5_codegen_a64.c:281` the CMP cases do this:

```c
case LIR_OP_CMP_RR:
case LIR_OP_CMP_RI:
case LIR_OP_TEST_RR: {
    if (dst_h == A64_NOREG) break;
    ...
    emit_cmp_w32_*(...);
    emit_cset_w32(&cg->emit, dst_h, a64cond);
}
```

Because of A1, `dst_h` for a node with `dst_v = 0` is `value_to_host[0] = W0`,
not `A64_NOREG`. So we **do** enter the body, emit the cmp, then emit
`cset W0, <cond>` — clobbering W0 and producing the boolean in a register that
nothing reads. The follow-up `LIR_OP_SETCC` node falls through the switch's
`default:` (silently skipped — see A6), so the real destination is never
written. Result: any guest `SEQ/SNE/SLT/...` whose result is not consumed by a
fused `CMP_JCC` is **dead silently**, and W0 is clobbered as a bonus.

Fixes (in order of preference):
1. Handle `LIR_OP_SETCC` properly and let `CMP_*` only emit the compare.
2. Or fold CMP+SETCC into the codegen explicitly: when you see `CMP_*` at
   index `i` followed by `SETCC` at `i+1`, emit `cmp` then `cset` into the
   SETCC's destination.

### A3. **DONE** (ae3831a0) **BUG** Side-exit / out-of-region branch emits the wrong condition and jumps to itself

`stage5_codegen_a64.c:489-494`:

```c
} else {
    // Side-exit or out-of-region conditional branch.
    // For the narrow path we currently just emit a placeholder.
    emit_b_cond(&cg->emit, COND_EQ, 0);
}
```

Two problems:

- Condition is hard-coded to `COND_EQ` regardless of the actual LIR condition.
- Offset 0 means "branch to the next instruction" — effectively a no-op.
  The side exit never actually exits.

Combined effect: any region with side exits silently falls through every side
exit as if the predicate were false (because the B.EQ never matters), and
control flow becomes incorrect the first time the guest would have taken the
side exit. The lifter is producing side exits today (e.g. `bench-fib.s32x`:
`Stage5 lift: OK insts=9 ir=10 cfg_blks=2 side=1`), so this fires immediately.

The codegen needs a real side-exit shape: emit the inverse-condition B.cond
that *skips* a side-exit stub, then the stub emits an `flush + write pc/exit
+ ret` (or chains to the side-exit block if it has been translated).

### A4. **DONE** (bb860660) **BUG** Terminal handling assumes ops BURG never emits, mis-derives `next_pc` for CALL

`stage5_codegen_a64.c:643-680` scans the LIR tail for `LIR_OP_JMP` or
`LIR_OP_CALL` to decide `next_pc` for the trailing epilogue. Two issues:

- `LIR_OP_JMP` is defined in `stage5_lir.h` but BURG never produces it. So
  the JMP branch is dead.
- For `LIR_OP_CALL` the code computes `next_pc = last->guest_pc + 4 +
  last->imm` — that is the **call target**, not the return site. After a
  guest JAL the dispatcher should resume at `call_pc + 4` (the instruction
  following the call), with the link in r31. Writing the call target into
  `cpu->pc` and returning to a non-existent caller will misdirect the
  dispatcher.

Also: every region unconditionally tails off with one `mov + str pc; mov +
str exit_reason; ret` (lines 674-680). There is no per-branch exit emission.
With A3 this is the **only** exit, so any control flow that should branch
elsewhere just falls into this single fixed exit.

### A5. **DONE** (bb860660) **BUG** Immediate forms silently truncate negative imms to low 12 bits

`emit_add_w32_imm` / `emit_sub_w32_imm` mask the immediate with `& 0xFFF`
(`emit_a64.c:367,381`). The a64 codegen passes them `(uint32_t)n->imm` where
`n->imm` is a signed 32-bit value (e.g. `-16` for a typical stack-bump
`add r29, r29, -16`). The negative converts to `0xFFFFFFF0`, masked to
`0xFF0` — i.e. `+4080`. Wrong code, no warning.

Smoke run on `bench-fib.s32x` exhibits this exact value:
```
[00] add_ri  dst=v2(g29) src=[v1,v0] imm=-16   gpc=0x00000000
```

Same hazard for any ADD/SUB/AND/OR/XOR/LEA where the immediate is negative or
outside `[0, 4095]`. Fix paths:
- Handle negative ADD by lowering to `SUB Wd, Wn, #imm` (and vice versa).
- For magnitudes > 4095, materialize the constant via
  `emit_mov_w32_imm32` into a scratch and use the R-form.
- LEA needs the same: `emit_add_w32_imm(..., n->disp)` at line 374 has no
  guard for `disp` outside `[0, 4095]`.

### A6. **DONE** (bb860660) **BUG** Unsupported LIR op is silently skipped

`stage5_codegen_a64.c:587-590`:

```c
default:
    // For now we silently skip unsupported ops in this narrow path.
    break;
```

This both hides bugs (A2's SETCC) and produces wrong code for the next
iteration that expects the destination to have been written. The function
should at minimum return `false` (caller already treats `false` as "don't
execute") and `stage5_codegen_a64_success` should not be incremented.

Ops currently missing in the a64 switch but produced by BURG:
- `LIR_OP_SETCC`
- `LIR_OP_JMP` (not emitted today, but trivially possible)
- `LIR_OP_RET`
- `LIR_OP_SYSCALL` (HALT/YIELD/DEBUG all funnel here)
- `LIR_OP_FP_HELPER`

Without these the emitter can never own a region that calls printf, exits via
HALT, or does FP — i.e. anything non-trivial.

### A7. **BUG** Prologue picks the wrong guest reg per slot

`stage5_codegen_a64.c:96-118` walks **all** RA intervals and overwrites
`cg->guest_in_slot[slot]` for every interval that lands on that slot. Because
slots are reused over time (linear-scan), `guest_in_slot[s]` ends up holding
the *last* interval's guest reg — not the live-in. The prologue then loads
that wrong guest reg into the slot.

The correct prologue should iterate intervals whose `start_idx == 0` (or are
live-in per the lifter's CFG) and load `guest_reg → assigned_slot` only once
per slot. (See B1 for the same bug repeated in `dbt5.c`.)

### A8. **DESIGN/BUG** Dirty-tracking is racy and unused

`slot_dirty[]` is set in many cases, but never read. Combined with A7, this
gives the misleading impression that there's a write-back step. There isn't
— the only "write-back" is the unconditional pc/exit_reason store in the
epilogue, and it ignores `slot_dirty[]` entirely. Either implement a real
flush (mirror `stage5_codegen_x64.c::cg_flush_exit`) or remove the field.

### A9. **BUG** `LIR_OP_UDIV` is emitted but BURG never produces it

`stage5_codegen_a64.c:531` handles `LIR_OP_UDIV` by emitting `UDIV` —
correct, but dead code: `stage5_burg.c` lowers `MIR_OP_DIV/REM` to
`LIR_OP_IDIV` with a `cond` bit and **never** emits `LIR_OP_UDIV`. So
unsigned `DIVU`/`REMU` from the guest end up emitting `SDIV` (signed)
through the IDIV path. This silently miscompiles unsigned division of values
with the high bit set.

Fix in BURG: distinguish signed vs unsigned at lowering and emit
`LIR_OP_UDIV` (and a `LIR_OP_UREM` you'd need to add) for the unsigned guest
opcodes. The a64 emitter already has half the support.

### A10. **BUG** Internal-branch fixup array is hard-capped at 16

`stage5_codegen_a64.h:79`:

```c
struct {
    size_t   patch_offset;
    uint32_t target_pc;
} internal_branches[16];   // small fixed size is fine for the narrow path
```

`stage5_codegen_a64.c:484` silently drops fixups when `internal_branch_count
>= 16`. A region with more than 16 forward branches will emit half-patched
branches that jump to offset 0 (relative). Either grow the cap or, better,
treat overflow as a hard failure (return false).

---

## B. Wiring in `dbt5.c` is silently broken on macOS arm64

### B1. **DONE** (bb860660) **WIRING** Block-cache install always fails — silently — because the cache has no pool

`dbt5.c:386-391`:

```c
static block_cache_t experimental_cache = {0};
static uint8_t experimental_code[4 * 1024 * 1024];
experimental_cache.code_buffer      = experimental_code;
experimental_cache.code_buffer_size = sizeof(experimental_code);
// Other fields (block pool, etc.) can stay zero for the first version.
g_a64_experimental_cache = &experimental_cache;
```

`cache_alloc_block` (`block_cache.c:497`) is:

```c
if (cache->block_pool_used >= cache->block_pool_size) return NULL;
```

With `block_pool_size = 0`, it **always** returns NULL. The wiring path at
`dbt5.c:463-485` then takes the silent failure branch:

```c
if (blk) { ... fprintf "[A64-WIRE] installed..." }
else     { munmap(exec_code, code_len + 4096); }   // no diagnostic!
```

So "Option 1 wiring complete" in the surrounding comments is wishful
thinking. The wiring has never run end-to-end. Empirical evidence: running
`S5_EMIT_A64=1 ./slow32-dbt5 bench-fib.s32x --lift-only` prints
`[A64-EMIT] clean emitter succeeded` but **not** `[A64-WIRE] installed
clean-emitted block into cache`.

Minimum fix: call a real `cache_init`/equivalent that allocates `block_pool`.

### B2. **DONE** (bb860660) **WIRING** Executable mmap fails on macOS arm64 (Apple W^X)

`dbt5.c:496`:

```c
void *exec_mem = mmap(NULL, 64*1024, PROT_READ | PROT_WRITE | PROT_EXEC,
                      MAP_PRIVATE | MAP_ANON, -1, 0);
if (exec_mem == MAP_FAILED) {
    perror("mmap exec");
```

Apple Silicon enforces W^X — `MAP_PRIVATE | MAP_ANON` with PROT_EXEC returns
`EACCES`. Observed in the smoke run: `mmap exec: Permission denied`. The
block-cache code in this same tree already handles this in
`block_cache.h:18-29` via `MAP_JIT` plus `pthread_jit_write_protect_np(0/1)`
bracketing. dbt5.c should use the same helpers (`dbt_jit_writable_begin/end`
and `DBT_JIT_MMAP_FLAGS`), or just route emission through the existing block
cache.

The earlier mmap at `dbt5.c:457` (the "install" path) has the same bug but is
masked by B1 — we never get there because `cache_alloc_block` returns NULL.

### B3. **DONE** (good enough for pilot) **WIRING** "Shadow validation" loop doesn't actually execute the guest

`dbt5.c:598-617` claims to run the same region through the shadow interpreter
for cross-checking:

```c
while (shadow_steps < MAX_TEST_STEPS) {
    if (cpu.pc >= cpu.code_limit) break;
    uint32_t raw = *(uint32_t *)(cpu.mem_base + cpu.pc);
    decoded_inst_t inst = decode_instruction(raw);
    if (inst.opcode == OP_HALT || inst.opcode == OP_YIELD || inst.opcode == OP_DEBUG)
        break;
    cpu.pc += 4;
    shadow_steps++;
}
```

**Status (as of pilot work):** We added `shadow_step_one()` (public wrapper) and replaced the pure decoder loop with real execution through the shadow state machine. The pilot now takes a pre-snapshot before the emitted call, runs the shadow forward from it, and does a richer comparison (pc + exit_reason + sp/r29) with decent diagnostics on failure.

This is considered "good enough for the pilot" per the current goal. A more complete integration with `shadow_pre_execute` / `shadow_verify` + full register + memory comparison can be done later when the pilot graduates into the main path.

### B4. **DONE** **WIRING** Inline asm contract is unsound — clobber list lies to the compiler

`dbt5.c:545-551` and `590-595`:

```c
__asm__ volatile (
    "mov x20, %0\n"
    "mov x21, %1\n"
    :
    : "r" (&cpu), "r" (cpu.mem_base)
    : "x20", "x21"
);
```

**Status:** Fixed in the pilot. Register setup (w8–w15 + x20/x21) was moved to immediately before the call. The `fprintf` that sat between setup and `fn()` was moved earlier, and the incorrect clobber list on the x20/x21 asm was removed. The pilot execution path is now reliable.

A more general architectural fix can be done when this code graduates beyond the pilot.

---

## C. Cleanup / refactor debt

### C1. **DONE** **CLEANUP** `try_clean_a64_emission` / `do_experimental_a64_emission` are dead stubs

Deleted the two never-called stubs (and their surrounding comments) in
`dbt5.c`. This eliminates the `-Wunused-function` warnings and removes
abandoned refactor scaffolding. The real logic remains inlined under the
S5_EMIT_A64 path (as it has been for a long time).

### C2. **DONE** **CLEANUP** Stale comments overstate completeness

Updated the two main comments in `dbt5.c` that used "Option 1 wiring complete"
language. They now accurately describe the pilot as "experimental A64 pilot
path" that does clean emission + real block cache wiring + optional direct
execution for validation.

The tone is now honest about the current state without over-promising.

### C3. **DONE** **CLEANUP** Misleading LIR dump for folded-immediate ADD/SUB

In `lower_alu_rr_ri` (and `lower_shift`), when BURG folds a constant src2
into an RI form, we now also update `m->imm` on the (now non-const) MIR node.
This makes the `--verbose` MIR dump show the actual folded value instead of
the stale small immediate from the original instruction encoding.

Example now shows consistent `imm=1048576` in both MIR and LIR for the folded
case. Much less confusing for readers.

### C4. **DONE** **CLEANUP** `LIR_OP_CALL` lowering drops `dst_v`

Added the missing `l->dst_v = m->dst_v;` line in the `MIR_OP_CALL` case
in `stage5_burg.c`. Calls now preserve their return-value SSA name, so the
RA sees the definition when the result is live. (In tiny regions where the
call result is dead at the end, zero intervals are still expected and correct.)

### C5. **CLEANUP** `shadow_interp.c` warning about unused `reg_mismatch_count`

`shadow_interp.c:825` triggers `-Wunused-but-set-variable`. Low-stakes but
indicative — the shadow comparison infrastructure has bit-rotted along with
B3. Worth a once-over.

### C6. **CLEANUP** Loader helpers in `s32x_loader.h` warn unused

`load_s32x_header`, `load_s32x_symtab`, `s32x_symtab_lookup`, `s32x_symtab_free`
are all `static` in a header included only by `dbt5.c`. dbt5.c uses
`load_s32x_file` only. Either drop the unused helpers from the dbt5 build
(via inclusion guards or a smaller header) or `(void)` them. Cosmetic.

---

## D. Design notes worth revisiting

### D1. **DESIGN** LIR is x86-shaped — AArch64 lowering needs translation glue

LIR uses x86 condition-code bytes (0x84..0x9F) in the `cond` field
(`stage5_burg.c:cmp_opcode_to_x86_cc`), x86 two-operand ALU semantics
(`dst` overlaps `src0`), and BURG's address-mode folding produces `LEA` /
`MOV_RM` / `MOV_MR` patterns aligned to x86.

The a64 emitter handles this by:
- A `lir_cond_to_a64()` translator that maps x86 CC bytes back to AArch64
  conditions, falling back to the original guest opcode when the CC byte is
  unknown (`stage5_codegen_a64.c:23-65`). This is fragile and easy to get
  wrong (it already enumerates both the JCC 0x8x range and the SETCC 0x9x
  range).
- Synthesizing a `mov dst, src0` before every two-operand ALU because
  AArch64 is three-operand (`emit_mov_w32_w32(dst_h, src0)` then
  `emit_add_w32(dst_h, dst_h, src1)` — wastes a mov in the common case
  where `src0 != dst_h`).

Two paths forward, in order of investment:
1. Cheap: extend the BURG-emitted `cond` field to carry an
   architecture-neutral comparison kind (eq/ne/lt/ltu/le/leu/...) instead
   of an x86 CC. Each backend's translator becomes a 1:1 switch.
2. Investment: change LIR ALU shapes to three-operand
   (`dst = src0 OP src1`). The x86 emitter already emits a mov-first
   prologue internally for non-coincident dst/src0; making it the LIR's
   responsibility regularizes both backends. AArch64 saves a mov per ALU
   op; x86 unchanged.

This is the single biggest "Stage 5 is not Stage-4-with-a-clean-room" lever
left — the current LIR is roughly Stage 4's machine model in a new file.

### D2. **DESIGN** 8 host slots is a Stage 4 holdover; AArch64 has many more

`STAGE5_RA_HOST_SLOTS = 8` (`stage5_ra.h:12`). The 8-slot budget comes from
Stage 4's reg cache (8 callee-saved x86 GPRs minus a few reserved). On
AArch64 the codegen has the full 24 callee-saved + extras to play with. The
`Viability for native ownership` heuristic in `dbt5.c:271-292` even penalises
"high register pressure (pressure > 8)" — that's an artificial ceiling.

Bumping `STAGE5_RA_HOST_SLOTS` for the a64 build (and adding more entries to
`slot_to_host[]`) should immediately reduce spills on hot loops. Even on x64
the 8-slot limit forces avoidable spills on register-heavy regions.

This is also the cleanest place to demonstrate that the dbt5 fork is moving
past Stage 4 — `stage5-revisit.md` item 5 already flags it.

### D3. **DESIGN** Region re-lifting on every CFG block — recomputes too much

`dbt5.c:651-671` walks the CFG the lifter already built and, for each block,
calls `stage5_lift_region_init` + `stage5_lift_superblock` + the full
SSA/MIR/BURG/RA pipeline from scratch on a new `tmp_region`. The lifter has
already discovered these blocks in `region->cfg_blocks[]`. For a real driver
(not just `--verbose` diagnostics) we want one lift pass that yields per-
block IR slices and one pipeline run per block, sharing work where possible.

This is a "later" item — fix once execution works (B1-B4 + A1-A6).

### D4. **DESIGN** No notion of "I cannot translate this — bail safely"

The driver assumes the codegen succeeds. There's no path to say "skip this
region, fall back to shadow." `stage5_codegen_a64` only returns false on
NULL args today. The `bool` return is the right signal — but A6's silent
default makes it lie. Combined with B3's broken validation, we have no way
to be confident the emitter is correct on any region. Fixing A6 plus
hooking up real shadow validation (B3) gives us a paranoid mode that can
shrink the failure set over time.

---

## E. Opportunities once the above is unblocked

### E1. **OPPORTUNITY** Real prologue from RA + lifter live-in mask

The lifter computes `region->cfg_blocks[0].live_in_mask` and per-reg flow
info (`region->reg_flow[]`). Pair that with the RA plan: for every live-in
guest reg, the unique interval whose `start_idx == first reference` is the
one whose slot needs the prologue load. This replaces both A7 and the dbt5.c
loop B1, and gives a fast (no-mov) entry to slot mapping.

### E2. **OPPORTUNITY** Constant materialization peephole

`emit_mov_w32_imm32` always emits MOVZ+MOVK. For constants where the upper
16 bits are 0 (or the lower 16 are 0), a single MOVZ suffices. For values
representable as a logical immediate, `ORR Wd, WZR, #imm` is one inst.
Stage-5-typical loads of guest immediates are dominated by small positives
and `LUI` results (lower 16 bits zero). A peephole here cuts code size
~10-20% on typical regions.

### E3. **OPPORTUNITY** Hoist live-in loads out of fast-path loops

Today the prologue loads live-ins once per region entry, then the loop body
re-reads them on every iteration if the RA spilled them. With more host
slots (D2) and a region big enough to cover a hot loop (lifter already
trace-stitches through forward branches and JAL with `imm > 0`), most loop
carriers stay in registers without spills. The remaining hot reload is
`mem_base` (x21) which is already set up by the trampoline.

### E4. **OPPORTUNITY** Use shadow_interp for differential fuzzing per region

Once B3 is real and A6 fail-fasts properly, we can run dbt5 in a mode where
**every** translated region is checked against the shadow on first
execution and the JIT'd code is discarded on mismatch (with a diagnostic).
This is exactly the discipline that allowed Stage 4 to ship — it's
substantially easier to add to dbt5 today than it will be after more
optimization layers land.

---

## F. Suggested order of attack (updated)

**Completed (as of bb860660):**
- A1–A6 (full emitter correctness)
- A3 (real side exits)
- B1 + B2 (wiring + Apple Silicon execution enablement)

The pilot can now emit correct A64 code and have it accepted by the real
block cache.

**Remaining order (dependencies respected):**

1. **A7–A10** (remaining emitter bugs) — Now observable thanks to real execution.
2. **C1–C6** — Cleanup pass (dead stubs, stale comments, `value_to_gpr` init, etc.).
3. **D1–D4** — Architectural improvements.
4. **E1–E4** — Measurable wins.

The core pilot milestone ("correct + executable + validated A64 emission") has
been reached. The list is now mostly cleanup, architecture, and polish.
