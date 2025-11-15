# Slow32 TCG Port — Working TODO

## Phase 0 — Repo Plumbing
- [x] Ensure `docs/slow32-tcg/` stays tracked; add links from `AGENTS.md` or `README.rst`.
- [ ] Decide where `.s32x` assets live for testing (e.g., `tests/tcg/slow32/` or reuse `slow-32/regression` outputs via relative paths).

## Phase 1 — Target Skeleton
- [x] Create `target/slow32/` directory by cloning the minimal structure from `target/openrisc/`.
- [x] Add `Kconfig` entry for `TARGET_SLOW32` (SoftMMU only to start).
- [x] Update `target/meson.build` and seed `configs/targets/slow32-softmmu.mak`.
- [x] Introduce `qemu-system-slow32` build target (`configs/devices/slow32-softmmu/default.mak`).

## Phase 2 — CPU State & Reset
- [x] Implement `Slow32CPUClass`/`Slow32CPU` in `cpu.c`/`cpu.h`.
- [x] Define register file, PC, MMIO base, stack pointer defaults, and helper accessors.
- [x] Hook reset logic to zero regs, set `pc` to entry point, `r29` to stack top, `mmio_base` from loader.
- [x] Add gdbstub register map (can piggyback on OpenRISC layout initially).

## Phase 3 — Loader & Machine
- [x] Write `hw/misc/slow32_loader.c` (or integrate into existing loader) that reads `.s32x`.
- [x] Create a simple machine in `hw/slow32/` (similar to `hw/openrisc/openrisc_sim.c`) that wires RAM and a basic DEBUG path (currently stdout; chardev binding TBD).
- [x] Support `-kernel program.s32x` to boot bare executables (presently expects raw binaries; `.s32x` parser pending).
- [ ] Expose loader knobs via machine properties (code size override, MMIO enable flag).

## Phase 4 — TCG Translation MVP
- [x] Implement decoder scaffolding in `translate.c` (fetch, field extraction, dispatch).
- [x] Cover core arithmetic/logical ops (`add/sub/and/or/xor`, shifts, immediates).
- [x] Implement load/store (byte/half/word, signed/unsigned).
- [x] Implement basic control flow (`jal`, `jalr`, `beq`, `bne`).
- [x] Add helpers for `debug`, `yield`, `halt`.
- [x] Run a trivial `.s32x` (e.g., hello world) and compare output with `slow32`.

## Phase 5 — ISA Completion
- [x] Add remaining branch variants (`blt/bge`, signed/unsigned).
- [x] Implement comparison-producing instructions (`slt`, `sle`, etc.).
- [x] Implement multiply/divide family (`mul`, `mulh`, `div`, `rem`). Start with helper calls mirroring `slow32.c`.
- [ ] Wire 64-bit helper sequences if needed for runtime builtins.
- [ ] Handle MMIO load/store paths once the device model exists.

## Phase 6 — Testing & Tooling
- [ ] Automate regression comparison: run `slow-32/regression/run-tests.sh`, then execute outputs under QEMU and diff logs.
- [ ] Add Avocado or meson `tests/tcg/slow32` cases with tiny programs checked into the repo.
- [ ] Document the workflow in `docs/system/target/slow32.rst` (build steps, limitations).
- [ ] Track performance vs. `slow32-fast` (optional stretch goal).
- [x] Provide a local benchmarking harness (`scripts/slow32/compare.py`) plus stats logging inside the CPU so we can measure translation start-up tax vs. steady-state execution.

Keep each checkbox tied to specific commits; update this file as scope evolves. Baby steps are fine—merge once a phase is functional, then iterate.
