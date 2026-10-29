# SLOW-32 Improvement Suggestions

This document consolidates feedback and improvement suggestions for the SLOW-32 toolchain components.

## Resolved Issues

### Assembler (all resolved)

- **Register names**: Case-insensitive via `tolower()` in `parse_register()` ✅
- **`.word symbol`**: REL_32 relocations for jump tables/data pointers ✅
- **Auto-align in `.text`**: `bump_size()` for uniform section accounting ✅
- **S-format parsing**: Explicit `rs2` handling with multiple addressing modes ✅
- **`%lo(symbol)`**: Correctly marked and applied in loads/stores ✅

### LLVM Backend (all resolved)

- **Switch/jump tables**: Custom `LowerJumpTable()`, assembler REL_32, linker support ✅
- **Address formation / GEP scaling**: `SelectAddr()` handles global+offset, 12-bit constraints ✅
- **Long branches**: R2 reserved for materialization, pseudo instructions expanded ✅
- **-O2 wrong output, two cfuzz finds** (2026-10-27; `regression/fuzz/cfuzz.py`
  seeds 5262 and 5406, where stage08 and the host compiler agreed and clang
  -O2 did not).  Two independent backend bugs:
  - `LowerSELECT`'s branchless mask, F ^ ((T ^ F) & -C), computes the arm the
    select discards; a combine used that arm's poison flags (`sub nuw 4, y`,
    from CorrelatedValuePropagation's `4 % y`) to fold 4 ^ (4 - y) into
    8 - y.  Both arms are frozen first now.
  - A virtual GPRPair was spilled/reloaded as two sub-register LDW_FI/STW_FI,
    each reload half an undef partial def, so the low half was dead to the
    allocator: the low word landed in r4 while the pair got r22:r23.
    `-verify-machineinstrs` rejected it all along.  Now one
    `LDW64_FI`/`STW64_FI`, split on the physical halves in
    `eliminateFrameIndex`.
  Guards: lit `select-poison-arm.ll`, `gprpair-spill-greedy.ll`; regression
  `bug-llvm-select-poison-arm`, `bug-llvm-gprpair-spill` (both at -O2) ✅
- **Machine verifier clean** (2026-10-27): with `-verify-machineinstrs`
  over the runtime, libcob, libf77 and SQLite, two more defects: an
  indirect call's explicit r31 def was live beside the implicit dead one
  (ten runtime files), and the optsize SELECT_PSEUDO inserter copied the
  head block's physical live-ins onto its new blocks (SQLite's
  slow32Open).  All of them verify now; cfuzz runs llc with the verifier
  and reports `clang-verify`.  Guards: lit `verify-indirect-call.ll`,
  `select-optsize-liveins.ll` ✅
- **`llvm.returnaddress(0)` after a call** (2026-10-28, `5055e0c4`): it
  copied physical r31 where used, i.e. the address the last call returned
  to; now the entry value via a virtual register. Nothing in the tree
  used it; the verifier found it under the shrink-wrap test ✅

### Toolchain (all resolved)

1. **64-bit Division** ✅ — `UMUL_LOHI` custom lowering verified correct.
2. **Linker Error Handling** ✅ — Hard errors on unresolved symbols (exit code 1).
3. **REL_CALL Implementation** ✅ — Implemented in `s32-ld.c`.
4. **Linker Range Checks** ✅ — LO12 signed ±2048, BRANCH +4094, JAL +1,048,574.
5. **Printf Formatting** ✅ — `%llu` and other formats verified.
6. **Runtime Safety & Optimization** ✅ — `memset` beq-only loops, segregated free list malloc.
7. **Emulator Safety** ✅ — W^X unified across all emulators, respects header flag.
8. **Shared Loader** ✅ — Callback-based `s32x_loader.h` used by all emulators.
9. **IEEE 754 Floating-Point** ✅ — Native f32/f64 instructions across entire toolchain.

### Runtime Libraries (all resolved)

1. **String-float conversions** ✅ — dtoa/strtod implemented.
2. **Math library transcendentals** ✅ — sin, cos, exp, log, etc. implemented in C (`runtime/math_soft.c`, compiled to the FP instructions); slow32-dbt and QEMU may replace them with host calls.

## Resolved — code layout / GCC parity (Jun 2026, Cascade Lake)

**cc-x64 reached GCC parity on `benchmark_core`: median 1.07s vs gcc 1.06s**
(it began this effort at 1.39x). The gap was code LAYOUT, not instruction
selection, and it was closed in-compiler by replicating the two things gcc does.

The diagnosis came from a layout *sweep* — `selfhost/tools/layout-sweep.sh`
(ours) + `gcc-layout-sweep.sh` (gcc), which pad `.text` across offsets and
compare the MEDIAN, defeating the ~25:1 layout:run-noise that makes any single
build A/B worthless. It showed gcc is *robust* (1.0% spread) where cc-x64 swung
20% — gcc is clever, not lucky. Two mechanisms, both now in cc-x64:

1. **Loop-head alignment** (`S32_LOOP_ALIGN`, default 32) — pins the hot
   `di->handler()` dispatch loop so its DSB (µop-cache) packing is invariant
   under `.text` shifts. gcc's `-falign-loops` analogue.
2. **Branch-straddle padding** (`S32_BRANCH32`, default on) — the Jcc erratum
   evicts any branch crossing a 32B line from the DSB; `x64_branch_pad()`
   (`x64_encode.h`) emits 1-byte NOPs so branches don't straddle. gcc's
   `-mbranches-within-32B-boundaries` analogue.

| build (pad sweep, REPS=15)   | spread | median |
|------------------------------|--------|--------|
| baseline off                 | 20.4%  | 1.25   |
| ALIGN=32 only                |  2.7%  | 1.13   |
| **BRANCH32 + ALIGN=32 (default)** | **4.7%** | **1.07** |
| gcc reference                |  1.0%  | 1.06   |

Source-robust (fixes every straddling branch, not one loop's lucky phase — an
overfit ALIGN=64/OFFSET=8 hit 1.08 and was deliberately NOT shipped);
behavior-neutral (27 cross-x64 tests byte-identical, checksum 0x8d70b2b);
cost +3% static `.text` NOPs (like gcc's). Disable: `S32_BRANCH32=0` /
`S32_LOOP_ALIGN=0`.

Possible refinements (not needed for parity): pad fused cmp+jcc as a unit
(gcc does; current pass pads the jcc alone); a64 loop alignment (needs
RPO-order back-edge detection); a post-link BOLT-style pass (heavier, robust
for all future codegen). None blocking — parity is reached.

## Open Items

- **DBT intrinsic-stub fault reporting** ✅ (2026-08 Pack B): A64
  `emit_a64_stub_fault_exit` stored `EXIT_REASON` into `exit_info` when
  `info_reg` was W0 (overwrote the fault address with e.g. 7). Fixed by
  writing `exit_info` before reusing W0 as scratch. Differential
  `bug-dbt-intrinsic-bounds*` addresses now match the reference.
- **QEMU fault reporting** ✅ (2026-10-03): qemu exited silently on the four
  `bug-dbt-intrinsic-bounds*` tests; fixed with the host-side pass
  (docs/SPEC.md 7.2). `run-differential.sh`'s `KNOWN_DIVERGENT` list is now
  empty.
- **QEMU guest exit codes** ✅: `helper.c` exits with guest `r1` via
  `qemu_system_shutdown_request_with_code` (checked 2026-10-28: a program
  returning 7 exits 7 under qemu, as under slow32). Since 2026-10-29
  `run-differential.sh` compares qemu's exit codes as well as its output
  (138/138 agree; a wrapper that rewrites qemu's 42 to 7 makes
  feature-exit-status diverge).
- **x64 DBT back-edge fix validation** ✅: the x86-64 DBT runs the
  differential harnesses on Linux x86-64; `eb49434c` (2026-08-25) fixed an
  x64-only back-edge bug found that way.
- Negative-input tests for the toolchain (malformed objects/archives/
  relocations) — the linker's hardened paths have no test coverage.
- See `docs/issues/` and the per-tool `ISSUES.md` files for older, lower
  priority items.

## Testing Recommendations

1. Test multi-file linking with global symbols ✅ (`feature-multifile`)
2. Test negative offsets in data access ✅ (`feature-negative-offset`)
3. Test loops with complex control flow ✅ (`feature-control-flow`)
4. Test all intrinsics with edge cases (0 size, odd sizes, etc.) ✅ (`stdlib-string-edge`)
5. Add string function regression tests (strlen, strcpy, memcpy, etc.) ✅ (`stdlib-string-funcs`)
6. Add stdlib regression tests (qsort, strtol, realloc) ✅ (`stdlib-qsort`, `stdlib-strtol`)
