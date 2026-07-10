# DBT Testing & Debugging Handbook

(Formerly the ARM/Intel handoff notes from the FP-encoding era; refreshed
2026-07-09. Historical findings from that handoff are folded into the
"History" section at the bottom. `ISSUES.md` is the live issue tracker.)

## Test Suites (all verified 2026-07-09)

```bash
# Differential vs. reference interpreter over regression/results/*.s32x
# (build the corpus first: cd regression && ./run-tests.sh)
tools/dbt/scripts/diff-test.sh

# SLOW BASIC (44 tests). EMU must be an ABSOLUTE path: the runner cd's
# into sbasic/ before invoking it, so a relative path silently runs nothing.
cd sbasic/tests && EMU="$PWD/../../tools/dbt/slow32-dbt" bash ./run-tests.sh

# Lua (11 tests). EMULATOR is overridable (absolute path for the same reason).
EMULATOR="$PWD/tools/dbt/slow32-dbt" bash lua/tests/run-tests.sh

# Forth sanity (23 test files)
cat forth/prelude.fth forth/tests/test-core-arith.fth | tools/dbt/slow32-dbt forth/kernel.s32x

# ISA-conformance oracle (expected values from an independent Python reference)
selfhost/isa-conformance/run-isa-conformance.sh

# Key performance workload (expected checksum: 0x8d70b2b)
tools/dbt/slow32-dbt ~/s32x/benchmark_core.s32x
```

## Flag Matrix for Triage

When a workload misbehaves, bisect the feature set. The 2026-07-09 back-edge
bug reproduced ONLY with the default config; every one of these masked it:

```bash
slow32-dbt -1|-2|-3 prog.s32x   # earlier stages
slow32-dbt -R prog.s32x         # no register cache
slow32-dbt -S prog.s32x         # no superblocks
```

If Stage 4 fails but `-R` and `-S` each pass, suspect a reg-cache x
superblock interaction (back-edges, deferred side exits, pending writes).

## Debugging Tools

```bash
# Lockstep shadow-interpreter verify of a DE-OPTIMIZED translation:
# --paranoid disables superblocks, reg cache, and peephole, so it cannot
# see bugs in those (the 2026-07-09 r13 corruption passed it cleanly).
slow32-dbt --paranoid prog.s32x

# Shadow-verify the PRODUCTION translation: all optimizations stay on,
# only chaining is disabled so every block execution returns to the
# dispatcher for comparison. The shadow follows each block's exact
# guest-PC footprint (superblock inlining, in-block back-edge loops) and
# runs its register file continuously so corruption propagates to a hard
# divergence instead of being absorbed by re-snapshotting. First runs
# found three real bugs: the a64 shifted-EOR fold miscompile, the x86
# JALR-fallback missing register flush, and (retroactively) the r13
# back-edge corruption. ~2.5s for all 285M insts of benchmark_core.
# Knobs: SLOW32_LITE_MAX_STEPS (shadow step budget per dispatch),
#        SLOW32_PARANOID_QUIET=1 (suppress the stats line for harnesses),
#        SLOW32_LITE_TRACE_SOFT=1 (trace register-only mismatches),
#        SLOW32_LITE_HARD_REGS=1 (escalate them to full reports).
# Note: on x86-64 lite disables intrinsic inlining (calls get inlined
# INTO blocks there); a64 keeps intrinsics (separate stub blocks, skipped
# by address). A natively-infinite loop inside one block can't be caught
# (no dispatcher return). Register-only mismatches are soft by design:
# dead-temp writeback skips make cpu->regs legitimately stale for dead
# values.
slow32-dbt --paranoid-lite prog.s32x

# Block-entry register trace (all 31 regs). Logs only dispatcher entries,
# i.e. first executions / chain misses — cached re-entries are silent even
# with NO_CHAIN... use budget + PC filter to narrow.
SLOW32_DBT_NO_CHAIN=1 SLOW32_DBT_TRACE_BLOCK_REGS_PC=0 \
  SLOW32_DBT_TRACE_BLOCK_REGS_MAX=999999 slow32-dbt prog.s32x

# One block's exits / side-exit table
SLOW32_DBT_TRACE_BLOCK_EXITS_PC=0xD8C0 slow32-dbt prog.s32x

# Per-instruction translation trace + raw emitted words for one block
SLOW32_DBT_EMIT_TRACE=1 SLOW32_DBT_EMIT_TRACE_PC=0xD8C0 slow32-dbt prog.s32x

# Exact guest-register corruption: hardware watchpoint on the guest reg file
gdb --args ./slow32-dbt prog.s32x
(gdb) break dbt.c:2346   # right after g_dbt_cpu is set
(gdb) run
(gdb) watch -l ((dbt_cpu_state_t*)g_dbt_cpu)->regs[13] if \
      ((dbt_cpu_state_t*)g_dbt_cpu)->regs[13] == 0x6f
```

The reference interpreter traces every guest instruction (PC + raw word):
`tools/emulator/slow32 -t prog.s32x`.

## Cross-ISA Testing from the AArch64 Box

The x86-64 translator (`translate.c`) builds and runs here via the cross
toolchain + qemu-user:

```bash
cd tools/dbt
make clean && make UNAME_M=x86_64 CC=x86_64-linux-gnu-gcc
qemu-x86_64-static -L /usr/x86_64-linux-gnu ./slow32-dbt prog.s32x
make clean && make        # restore the native aarch64 build
```

## Diagnostic Builds

```bash
make clean && make CFLAGS="-O0 -g -fsanitize=address,undefined -fno-omit-frame-pointer"
make clean && make CFLAGS="-O2 -g -fno-strict-aliasing"
```

## Triage Heuristics

- Issue on one architecture/compiler only → suspect UB or codegen-sensitive C
  in the DBT itself first.
- Issue reproduces in DBT *and* interpreter → suspect the guest program or
  shared runtime, not translation.
- Stage 4-only issue → flag matrix above; then block-regs trace to find the
  first block entered with bad state; then the gdb watchpoint to find the
  emitted store that wrote it.

## History (from the original ARM/Intel handoff, ~May 2026)

- `emit_a64.c` AArch64 FP 1-source encodings (`fneg/fabs/fsqrt`) were wrong;
  symptom was negative FP values losing sign (`-7.30 -> 7.30`). Fixed.
- Memory-fault signatures around the `free` path (`PC=0x0001A1E8`) reproduced
  in the non-DBT interpreters too (guest-side, not a translation bug).
- `sbasic/tests/run-tests.sh` filters DBT memory-fault text the same way it
  filters fast-emulator OOB lines, so harness output aligns across emulators.
- The `lua-triage.sh` smoke matrix referenced by the old version of this file
  no longer exists; use the Lua suite command above.
