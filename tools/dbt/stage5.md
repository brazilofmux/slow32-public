# SLOW-32 DBT Stage 5 Plan

This document defines the initial implementation plan for `slow32-dbt -5`.

## Goal

Stage 5 is an experimental optimization stage built on top of Stage 4:
- Keep Stage 4 correctness and dispatch behavior.
- Add lifted superblock selection and BURG-based instruction selection.
- Preserve a hard fallback path to existing Stage 4 translation.

## Non-Goals (Initial Slice)

- No full-function/global optimization.
- No mandatory replacement of existing Stage 4 emitter.
- No floating-point/i64-specific BURG coverage in initial milestone.
- No cross-block global register allocation.

## Stage 5 Contract

- `-5` must run all programs that `-4` runs.
- Any unsupported or uncertain optimization path must fall back to Stage 4 emission.
- Carry-sensitive behavior (unsigned compare/branch semantics) must be conservative by default.
- Side exits, chaining, RAS, and inline lookup behavior must remain compatible with Stage 4.

## Execution Model

1. Translate block/superblock as Stage 4 currently does.
2. For eligible region(s), lift guest instructions into a small IR tree/DAG.
3. Run BURG selector on lifted region.
4. Emit selected host sequence.
5. Emit uncovered parts through existing Stage 4 path.
6. If anything fails at any step, emit entire region via Stage 4 fallback.

## Phased Plan

## Phase 0: Safety + Telemetry (done/in progress)

- Stage `-5` mode exists and remains Stage 4-compatible.
- Add strict carry guardrail defaults for sensitive fusion paths.
- Add counters to measure optimization attempts/skips/fallbacks.

## Phase 1: Lift IR (Minimal Superblock Slice)

Build a minimal lifted representation for integer hot paths:
- Arithmetic: `add/sub/and/or/xor`, shifts.
- Comparisons: signed/unsigned compare forms.
- Branch conditions and control edges.
- Address forms for common load/store patterns.

Requirements:
- Explicitly represent signed vs unsigned compare semantics.
- Preserve exact branch target/fallthrough mapping.
- Keep region boundaries aligned with current side-exit model.

### Phase 1 Task Checklist

- [ ] Add Stage 5 lift module skeleton:
  - `tools/dbt/stage5_lift.h`
  - `tools/dbt/stage5_lift.c`
  - Define minimal IR node kinds and region container types.

- [ ] Add Stage 5 BURG module skeleton:
  - `tools/dbt/stage5_burg.h`
  - `tools/dbt/stage5_burg.c`
  - Start with API stubs that can return "no match" cleanly.

- [ ] Wire Stage 5 path in dispatcher-level translation flow:
  - `tools/dbt/dbt.c`
  - `tools/dbt/translate.c`
  - Add a disabled-by-default Stage 5 sub-flag/toggle for BURG selection path.

- [ ] Define minimal lifted IR nodes (integer-only first slice):
  - constants, guest-reg reads/writes
  - `add/sub/and/or/xor`
  - shifts (`sll/srl/sra`)
  - compare nodes (signed and unsigned)
  - branch condition node (taken/fallthrough mapping)
  - load/store address-form node

- [ ] Implement region lifting for one superblock candidate:
  - lift sequential instructions until boundary (branch/side-exit/cap)
  - preserve mapping to original guest PCs
  - record unsupported opcodes and abort-to-fallback reasons

- [ ] Add strict semantic flags in lifted compare nodes:
  - signed/unsigned bit
  - immediate/register operand form
  - branch predicate kind

- [ ] Add validation checks before BURG:
  - region must be structurally complete
  - no unresolved values
  - no unsupported side effects in selected slice
  - otherwise fallback to Stage 4 emission

- [ ] Add Stage 5 counters:
  - `stage5_lift_attempted`
  - `stage5_lift_success`
  - `stage5_lift_fallback_<reason>`
  - print in DBT `-s` stats

- [ ] Add focused carry/unsigned smoke inputs in regression or DBT-local tests:
  - unsigned compare+branch edge cases
  - verify parity across interpreter / `-4` / `-5`

- [ ] Keep Makefile integration minimal:
  - update `tools/dbt/Makefile` to compile/link new Stage 5 files
  - ensure build still succeeds when Stage 5 BURG path is disabled

### Phase 1 Acceptance Criteria

- [ ] `make -C tools/dbt` builds cleanly.
- [ ] `slow32-dbt -5` runs with no regressions vs `-4` when BURG path disabled.
- [ ] Lift path can be enabled and produces deterministic fallback reasons.
- [ ] Unsigned/carry-sensitive tests pass with strict-carry guardrails enabled.

## Phase 2: BURG Selector (Minimal Pattern Set)

Introduce BURG matching for high-value local patterns first:
- Compare + branch chains.
- Address computation + memory op patterns.
- Constant materialization simplifications.
- Small arithmetic chains.

Requirements:
- Cost model includes at least code size and simple execution cost.
- Deterministic selection.
- Clean fail/abort path to Stage 4 emitter.

## Phase 3: Local Register Assignment for Selected Regions

Add region-local register assignment over BURG output:
- Respect DBT host ABI conventions and clobber rules.
- Spill conservatively when pressure exceeds capacity.
- Keep interfaces compatible with existing emit/exit machinery.

Requirements:
- No global/lifetime-spanning RA across unrelated blocks.
- Never block fallback when allocation fails.

## Phase 4: Integration and Mixed Emission

Support mixed codegen within one translated region:
- BURG-selected emission for covered subsequences.
- Stage 4 emission for unsupported instructions/subsequences.
- Preserve Stage 4 branch chaining and side-exit patching.

Requirements:
- Stable patch-site accounting and exit metadata.
- No changes to external cache format unless versioned/migrated safely.

## Phase 5: Correctness Harness and Rollout

Add differential validation:
- Compare interpreter vs `-4` vs `-5`.
- Add targeted carry/unsigned branch stress tests.
- Optional debug mode: dual-execute selected regions and compare architectural state at exits.

Rollout:
- Keep BURG path behind a dedicated Stage 5 sub-flag (or env toggle) initially.
- Expand pattern coverage by measured hit rates and verified correctness.

## Guardrails

- If lift fails: fallback.
- If BURG has no complete/legal cover: fallback.
- If RA fails: fallback.
- If patching invariants are not satisfied: fallback.
- All fallbacks should be counted and categorized for visibility.

## Suggested Metrics

- `stage5_lift_attempted`
- `stage5_lift_success`
- `stage5_burg_attempted`
- `stage5_burg_selected`
- `stage5_burg_selected_guest_insts`
- `stage5_burg_fallback_total`
- `stage5_burg_fallback_<reason>`
- `stage5_emit_success_guest_insts`
- `stage5_emit_success_host_bytes`
- per-pattern emit efficiency (`emit pattern <name> ... bpg=<x>`)
  - terminals are split (`jal_call_short`, `jal_call_long`, `jal_jump`, `jalr_ret_short`, `jalr_ret_long`, `jalr_indirect`, `halt`, `yield`, `debug`)
  - direct branches are split (`direct_branch_eq`, `direct_branch_ne`, `direct_branch_rel`, `direct_branch_relu`)
  - `jal_call_short`, `jal_call_long`, `jalr_ret_short`, `jalr_ret_long`, `jalr_indirect`, `direct_branch_eq`, `direct_branch_rel`, and `direct_branch_relu` currently use policy fallback to Stage 4 emission
  - `jal_jump` is enabled only for single-instruction selected regions (`guest_inst_count == 1`) and otherwise falls back to Stage 4
- `stage5_strict_carry_skips`

### Quick Comparison Workflow

Use the helper script to get parity + coarse runtime and Stage5 coverage metrics:

```bash
tools/dbt/scripts/compare-stage4-stage5.sh
```

Or pass specific `.s32x` files:

```bash
tools/dbt/scripts/compare-stage4-stage5.sh \
  regression/results/feature-strtod/test.s32x \
  regression/results/feature-crc32/test.s32x
```

To rank BURG pattern hotspots across many tests:

```bash
tools/dbt/scripts/stage5-aggregate-patterns.sh
```

## Initial Test Focus

- Unsigned compare/branch correctness:
  - `SLTU/SLTIU/SGTU/SLEU/SGEU`
  - `BLTU/BGEU` paths with edge-case values (`0`, `1`, `0x7fffffff`, `0x80000000`, `0xffffffff`)
- Superblock side-exit behavior parity with Stage 4.
- Regression parity for representative toolchain workloads.

## Milestone Exit Criteria (Initial)

Stage 5 is ready for broader use when:
- Differential tests show no semantic deltas vs Stage 4/interpreter on target suites.
- Fallback rate is understood and decreasing as patterns expand.
- Performance is neutral-to-positive on selected workloads, with correctness maintained.
