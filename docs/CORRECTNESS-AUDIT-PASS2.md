# Pass 2 (CORRECTNESS) Audit Report — SLOW-32 Toolchain

> **Status update (Jul 2026): all nine findings are FIXED.** Findings 1–7
> were fixed after this report was written but their status lines were not
> updated at the time (only 8–9 were). Per-finding status notes below record
> where each fix landed. NaN comparison coverage now exists as
> `regression/tests/float-nan-compare`.

## 1. Executive Summary

Pass 2 confirmed **11 distinct correctness defects** that survived two-lens verification (spec/cross-implementation plus a concrete counterexample). The damage clusters overwhelmingly in **floating-point comparison lowering** in the LLVM backend: six of the eleven findings are NaN-handling bugs in `SLOW32InstrInfo.td` where unordered/ordered FP predicates are lowered to bare hardware compares, silently miscompiling any comparison whose operands can be NaN. The remaining defects are spread across the assembler (`not` pseudo-instruction and `%hi` relocations on I/S-type operands), the linker (synthesized fallback symbols missing their section base address), the interpreter (a `JALR` ordering bug for `rd == rs1`), and one cost-model inaccuracy. As a stability baseline, the **four emulators (interp, slow32-fast, slow32-dbt, QEMU) agree on all non-interactive prebuilt programs**; the only emulator divergence found is the interpreter-only `JALR` bug, which does not arise from normal compiler output.

## 2. Severity-Ranked Findings

| # | Severity | Layer | File:line | Title |
|---|----------|-------|-----------|-------|
| 1 | High | LLVM isel | SLOW32InstrInfo.td:280 / :335 | Unordered FP relational compares (`setult/setule/setugt/setuge`, f32 & f64) return FALSE on NaN |
| 2 | High | LLVM isel | SLOW32InstrInfo.td:277 / :331 | `setueq` (unordered-equal, f32 & f64) returns FALSE on NaN |
| 3 | High | LLVM isel | SLOW32InstrInfo.td:263 / :313 | `setone` (ordered not-equal, f32 & f64) returns TRUE on NaN |
| 4 | High | Assembler | slow32asm.c:2169 | `not rd, rs` pseudo zero-extends XORI: computes `rs ^ 0xFFF` not `rs ^ 0xFFFFFFFF` |
| 5 | High | Emulator (interp) | slow32.c:644 | `JALR` writes link register before computing target, breaking `rd == rs1` |
| 6 | Medium | Assembler | slow32asm.c:2376 | `%hi`/`%pcrel_hi` on I/S-type operand silently emits REL_32, overwriting whole instruction word |
| 7 | Medium | Linker | s32-ld.c:1501 | Synthesized fallback symbol gets section-relative value (missing section vaddr) |
| 8 | Low | LLVM isel | SLOW32ISelLowering.cpp:1753 | `isLegalAddressingMode` reports ±32767 offsets legal but encoding is simm12 (±2048) |
| 9 | Low | LLVM MC | SLOW32MCCodeEmitter.cpp:584 | `LI`/`ORI` materialization silently zero-truncates immediates to 12 bits |

Note: findings #1–#3 and #6 each pair a verified f32 and f64 (or near-identical) instance; they are deduplicated into single entries below with both line ranges called out.

---

## 3. Findings by Severity

### HIGH

---

#### Finding 1 — Unordered FP relational compares return FALSE on NaN (f32 & f64)
**Location:** `llvm-backend/SLOW32/SLOW32InstrInfo.td:280-283` (f32), `:335-342` (f64)

**Wrong vs correct.** The unordered predicates lower directly to ordered hardware compares with no NaN term: `setult→FLT(a,b)`, `setule→FLE(a,b)`, `setugt→FLT(b,a)`, `setuge→FLE(b,a)`. `FLT`/`FLE` return 0 when an operand is NaN, but an *unordered* predicate must return **1** whenever either operand is NaN. Correct lowering ORs the ordered result with a NaN test, e.g. `setult(a,b) = XORI(FLE(b,a),1)`, or mark `setCondCodeAction(...Expand)` and let LLVM legalize.

**Counterexample.** `int f(double a, double b){ return !(a < b); }`. Clang lowers `!(a<b)` to `fcmp uge` (`getInversePredicate(FCMP_OLT)==FCMP_UGE`), which selects `FLE_D(b,a)`. For `a=NaN, b=1.0` the source result is `!(NaN<1.0)=!false=1`, but `FLE_D(1.0,NaN)=0`, so the function returns 0 — a silent miscompile, reachable at `-O2` with **no** `-ffast-math`.

**Evidence.** `docs/INSTRUCTION-SET.md:157` ("Comparisons with NaN operands return 0"); emulator `slow32.c:739-751` (f32) and `:857-867` (f64) implement `FLT`/`FLE` as C `<`/`<=`; `slow32-fast.c` agrees. `ISD::SETCC` is Legal for f32/f64 (`SLOW32ISelLowering.cpp:382,405`) with **no** `setCondCodeAction` override, so these `.td` patterns are the sole lowering, and `LowerBRCOND` deliberately routes float SETCC through them rather than decomposing.

**Fix.** Rewrite the four unordered relational f32/f64 patterns as the `XORI`-with-1 complement of the opposite ordered compare (e.g. `setuge(a,b) → (XORI (FLT a,b) 1)`), matching the existing `setune` style, **or** call `setCondCodeAction(ISD::SETULT/SETULE/SETUGT/SETUGE, MVT::f32/f64, Expand)`.

**Status: FIXED.** All four unordered relationals are now the XORI-complement of the opposite ordered compare (`SLOW32InstrInfo.td:288-291` f32, `:347-354` f64). Verified at runtime by `regression/tests/float-nan-compare`.

---

#### Finding 2 — `setueq` (unordered-equal) returns FALSE on NaN (f32 & f64)
**Location:** `llvm-backend/SLOW32/SLOW32InstrInfo.td:277` (f32), `:331-332` (f64)

**Wrong vs correct.** `setueq(a,b)` is selected as bare `FEQ_S`/`FEQ_D`. `FEQ` is ordered equality (0 on NaN), but `setueq` is unordered-equal: true when `a==b` **OR** either operand is NaN. Correct: `setueq(a,b) = XORI(OR(FLT(a,b),FLT(b,a)),1)`, or `OR(FEQ(a,b), unordered-test)`.

**Counterexample.** `setueq(NaN, NaN)` must be 1, but `FEQ_S(NaN,NaN)=0` yields 0. Likewise `setueq(NaN, 5.0)` returns 0 instead of 1.

**Evidence.** Emulator `FEQ_S/FEQ_D` use C `==` (`slow32.c:736`, `:854`), false for any NaN. `SETCC` Legal (`:382,405`) with no condition-code expansion, so this pattern is the sole lowering of `FCMP_UEQ`. The adjacent `seto/setuo` patterns (`:270-275`, `:324-329`) already use the FEQ self-equality NaN idiom, proving the technique was available; the line-276 comment "NaN handling is architectural" is the mistaken rationale. Note `setune` (`:278/:333`) `= XORI(FEQ,1)` is correctly the unordered inverse and is **not** affected.

**Fix.** Lower `setueq` as `(XORI (OR (FLT a,b) (FLT b,a)) 1)` for f32/f64, or `setCondCodeAction(ISD::SETUEQ, MVT::f32/f64, Expand)`.

**Status: FIXED.** `setueq` now lowers as `(XORI (OR (FLT a,b) (FLT b,a)) 1)` (`SLOW32InstrInfo.td:283-285` f32, `:342-344` f64). Verified by `regression/tests/float-nan-compare`.

---

#### Finding 3 — `setone` (ordered not-equal) returns TRUE on NaN (f32 & f64)
**Location:** `llvm-backend/SLOW32/SLOW32InstrInfo.td:263-264` (f32), `:313-314` (f64)

**Wrong vs correct.** `setone(a,b)` is lowered to `XORI(FEQ(a,b),1)` — which is `!(a==b ordered)`, i.e. *unordered*-not-equal (`setune`) semantics, true on NaN. But `setone` is *ordered* not-equal: it must be **0** when either operand is NaN. Correct: `setone(a,b) = OR(FLT(a,b), FLT(b,a))` (both `FLT` return 0 on NaN, so the result is 0).

**Counterexample.** `setone(NaN, 1.0)` must be 0 (operands unordered), but `XORI(FEQ_S(NaN,1.0)=0, 1) = 1` — returns 1. The backend gave `setone` the identical expansion as `setune`, which is wrong precisely because they differ on NaN.

**Evidence.** `FEQ` ordered (`slow32.c:736/:854`), `docs/INSTRUCTION-SET.md:157`. LLVM distinguishes `FCMP_ONE` from `FCMP_UNE`. `setone` arises from ordinary ordered idioms like `(a<b)||(a>b)` and `__builtin_islessgreater`. `SETCC` Legal with no expansion (`:382,405`), so the pattern is reached directly.

**Fix.** Lower `setone(a,b)` as `(OR (FLT a,b) (FLT b,a))` for f32/f64, or `setCondCodeAction(ISD::SETONE, MVT::f32/f64, Expand)`.

**Status: FIXED.** `setone` now lowers as `(OR (FLT a,b) (FLT b,a))` (`SLOW32InstrInfo.td:265-267` f32, `:322-324` f64). Verified by `regression/tests/float-nan-compare`.

---

#### Finding 4 — `not rd, rs` pseudo miscompiles to `rs ^ 0xFFF`
**Location:** `tools/assembler/slow32asm.c:2169`

**Wrong vs correct.** `not rd, rs` expands to `xori rd, rs, -1`. `encode_i` classifies XORI (0x1E) as zero-extended and stores `(imm & 0xFFF) << 20`, so the immediate becomes `0xFFF`. Both emulators zero-extend XORI, so the instruction computes `rs ^ 0x00000FFF` — only the low 12 bits are inverted — instead of the intended `rs ^ 0xFFFFFFFF`. The all-ones mask is simply not representable in a zero-extended 12-bit field.

**Counterexample.** `addi r3, r0, 5` / `not r4, r3` assembles to `0xfff1821e`; under `slow32 -r` it yields `r4: 0x00000005 -> 0x00000FFA` (`5 ^ 0xFFF`) instead of the correct `0xFFFFFFFA` (`~5`). The assembler emits a "truncated to 12-bit unsigned range" warning but still produces the broken instruction.

**Evidence.** `docs/INSTRUCTION-SET.md:264` defines the pseudo as `xori rd, rs, -1`, but line 79 defines XORI as zero-extended `rd = rs1 ^ imm`, so the documented expansion is itself unsound. The disassembler sign-extends I-type immediates (`slow32dis.c:188`), so it misleadingly prints `xori r4, r3, -1`, masking the bug — a disassembler/CPU disagreement. The project's own `MEMORY.md` already records "xori with -1 immediate broken … use addi+xor instead."

**Fix.** Do not expand `not` to `xori rd, rs, -1`. Use the two's-complement identity (`sub rd, r0, rs; addi rd, rd, -1`, both sign-correct), or load `-1` into a scratch via a sign-extended `addi` and use register `xor`. Also correct the spec table at line 264.

**Status: FIXED.** `not` now expands to `sub rd, r0, rs; addi rd, rd, -1` (`slow32asm.c`, `"not"` pseudo handler) and the spec table row in `docs/INSTRUCTION-SET.md` documents the new expansion.

---

#### Finding 5 — Interpreter `JALR` corrupts target when `rd == rs1`
**Location:** `tools/emulator/slow32.c:644`

**Wrong vs correct.** The handler executes `cpu->regs[inst.rd] = cpu->pc + 4;` **before** reading rs1 to form the target (`next_pc = (cpu->regs[inst.rs1] + inst.imm) & ~1;`). When `rd == rs1`, the link write clobbers rs1, so the target becomes `(pc+4)+imm` instead of `old_rs1+imm`. The target must be computed from the original rs1 value, independent of the rd write.

**Counterexample.** `la r5, target; jalr r5, r5, 0; addi r1,r0,99; halt; target: addi r1,r0,42; halt`. The interpreter exits **99** (fell through to pc+4), while `slow32-fast` and `slow32-dbt` exit **42** (correctly jumped to target). This is the only emulator divergence found across the baseline.

**Evidence.** `docs/INSTRUCTION-SET.md:125` (`rd = PC+4; PC = rs1 + imm`). `slow32-fast.c:704-708` computes `target` from rs1 **first**, then writes `regs[rd]`; `tools/dbt/translate.c:2727-2764` loads rs1 into RAX first, with an explicit comment that the return-address store must not clobber the saved target. The interpreter is the lone outlier.

**Fix.** Snapshot the target before writing the link register:
```c
case OP_JALR: {
    uint32_t target = (cpu->regs[inst.rs1] + inst.imm) & ~1u;
    cpu->regs[inst.rd] = cpu->pc + 4;
    next_pc = target;
    break;
}
```
The trigger is narrow (the standard ABI uses distinct link/target registers, e.g. `jalr r31, r2, 0`), so normal compiler output is unaffected — but hand-written asm reusing one register for both link and target silently mis-executes on the reference/debug emulator.

**Status: FIXED.** The interpreter now snapshots the target before writing the link register (`slow32.c` `OP_JALR` case), restoring agreement with slow32-fast and the DBT.

---

### MEDIUM

---

#### Finding 6 — `%hi`/`%pcrel_hi` on I/S-type operand silently emits REL_32
**Location:** `tools/assembler/slow32asm.c:2376` (FMT_I), `:2426` (FMT_S)

**Wrong vs correct.** The FMT_I and FMT_S operand handlers copy only `res.is_lo`/`res.is_pcrel_lo` onto the instruction; `res.is_hi`/`res.is_pcrel_hi` are never captured (the FMT_U handler at `:2525-2526` does capture both). With no hi/lo flag set, the reloc-type selection (`:3205-3216`) falls through to `S32O_REL_32`. The linker's REL_32 handler (`s32-ld.c:1795`) does `*target = value`, overwriting the **entire** 32-bit instruction word (opcode, rd, rs1) with the symbol address. A `%hi`/`%pcrel_hi` on an I/S operand must emit `S32O_REL_HI20`/`REL_PCREL_HI20` (which patch only the upper bits), or be rejected with an explicit error.

**Counterexample.** `addi r3, r0, %hi(foo)` (foo in `.data`) emits a type-32 relocation for foo at text offset 0. After linking, the addi encoding `0x00000190` is clobbered to `0x00001000` (foo's data base), which disassembles to `add zero, zero, zero` — opcode/rd/rs1 all destroyed.

**Evidence.** FMT_U (`:2525-2526`) vs FMT_I (`:2376-2377`) / FMT_S (`:2426-2427`) asymmetry; reloc selection at `:3205-3216` has no hi path; linker REL_HI20 (`s32-ld.c:1814`) preserves low 12 bits, REL_32 (`:1795`) does not. `docs/file-formats.md` documents HI20 as partial-patch ("Write upper 20 bits to LUI"). Reachability is low: LLVM and the in-tree backends only emit `%hi`/`%pcrel_hi` on LUI/AUIPC (FMT_U), so this triggers only on hand-written/non-conforming assembly.

**Fix.** In the FMT_I and FMT_S symbol branches, also copy `inst->symbol_is_hi = res.is_hi;` and `inst->symbol_is_pcrel_hi = res.is_pcrel_hi;` (mirroring FMT_U). Alternatively, detect `res.is_hi || res.is_pcrel_hi` in I/S context and emit an explicit error instead of degrading to REL_32.

**Status: FIXED.** The explicit-error alternative was taken: `reject_hi_on_imm12()` (`slow32asm.c:1322`) rejects `%hi`/`%pcrel_hi` in all FMT_I and FMT_S symbol branches.

---

#### Finding 7 — Synthesized fallback symbol gets section-relative value (missing section vaddr)
**Location:** `tools/linker/s32-ld.c:1501`

**Wrong vs correct.** When `collect_relocations()` references a symbol absent from the combined table, it synthesizes an entry with a section-**relative** value: `nsym->value = isym->value + inf->section_base[isym->section-1]`. But `collect_relocations()` runs **after** `update_symbol_values()` (main: `:2576` then `:2580`), and it is `update_symbol_values()` that adds `sec->vaddr` to make values absolute. The synthesized symbol therefore never gets its section base. Additionally, the section lookup at `:1502-1503` uses the **raw** input section name instead of `canonical_section_name()`, so a subsection like `.rodata.foo` misses the section map (`section_idx = -1`). A synthesized defined symbol must resolve to the same absolute VA as the normal `build_symbol_table()`+`update_symbol_values()` path: `combined_section.vaddr + section_base + value`.

**Counterexample.** A `.rodata` local at section offset 0 with `rodata_base=0x1000`: the normal path yields `0x1000`, but the synth path yields `0x00000000`. A `.word sym` / `la rX, sym` against it then resolves to `0x0` (code region) instead of `0x1000`. For `.text` the bug is masked because `code_base/vaddr = 0`.

**Evidence.** `build_symbol_table()` sets section-relative value (`:844/:895`) and `update_symbol_values()` (`:1720: sym->value = sec->vaddr + sym->value`) converts to absolute; the synth path duplicates only the relative half and runs too late. `apply_relocations()` (`:1790`) uses `sym->value` verbatim. `build_symbol_table` uses `canonical_section_name()` (`:899`) where the synth path uses the raw name (`:1502`). **Reachability caveat:** verifiers found the fallback fires **zero times** across the regression suite and across strip-linking all 60 in-repo `.s32o` files, because `build_symbol_table` retains any relocation-referenced local even under `-s`. The bug is latent/defense-in-depth — it would manifest only under a future invariant change or a foreign object producer.

**Fix.** In the synth path, look up the combined section via `canonical_section_name(sec_nm)`; if found (`cs>=0`), set `nsym->section_idx = cs` and `nsym->value = ld->sections[cs].vaddr + inf->section_base[isym->section-1] + isym->value`.

**Status: FIXED.** The synth path now resolves the combined section via `canonical_section_name()` and makes the value absolute with the section vaddr (`s32-ld.c:1500-1511`).

---

### LOW

---

#### Finding 8 — `isLegalAddressingMode` over-reports legal offset range
**Location:** `llvm-backend/SLOW32/SLOW32ISelLowering.cpp:1753`

**Wrong vs correct.** The function accepts `AM.BaseOffs` in `[-32768, 32767]` (16-bit), and its comment even says "fits in 16-bit signed immediate," but the actual load/store offset operand is `simm12` (`isInt<12>`, `-2048..2047`; `SLOW32InstrInfo.td:12-14`, LDW/STW at `:164-171`). The reported legal range must match the simm12 encoding. **No miscompile results:** `SelectAddr` (`SLOW32ISelDAGToDAG.cpp:91,107`) independently guards folding with `isInt<12>` and falls back to a separate ADD, so an over-range offset is correctly materialized — the only effect is degraded code (LSR/CGP cost mismodeling).

**Counterexample.** `base[i]` with constant displacement 8000: `isLegalAddressingMode` returns true, but `SelectAddr`'s `isInt<12>` rejects the fold at isel, so an extra ADD is emitted. Correct result, worse code.

**Fix.** Tighten the bound to `if (AM.BaseOffs < -2048 || AM.BaseOffs > 2047) return false;` and fix the misleading comment.

**Status: FIXED.** Bound tightened to simm12 (`±2048`) and comment corrected in `SLOW32ISelLowering.cpp`. Build clean, regression 61/61.

---

#### Finding 9 — `LI`/`ORI` materialization silently zero-truncates immediates to 12 bits
**Location:** `llvm-backend/SLOW32/MCTargetDesc/SLOW32MCCodeEmitter.cpp:584`

**Wrong vs correct.** The `SLOW32::LI` case encodes `ori rd, r0, imm` via `encodeIImmediate`, which masks to `imm & 0xFFF` (zero-extended). So `LI rd, -1` produces `4095` and `LI rd, 5000` produces `904` — upper/sign bits dropped, with no range check. A branch-compare constant must be materialized with its full signed 32-bit value (the `ISD::Constant` path at `SLOW32ISelDAGToDAG.cpp:204-246` does this correctly via ADDI/LUI+ADDI). **Latent, not live today:** `ISD::BRCOND` is Custom-lowered (`SLOW32ISelLowering.cpp:226`) via `emitBranchForCond` into BR_* nodes that take **GPR** operands, so the constant RHS flows through the sign-correct `ISD::Constant` path. The unconstrained `(LI imm:$b)` brcond patterns at `SLOW32InstrInfo.td:654-676` are dead/shadowed; the grep showed no other live consumer of `LI` with an out-of-`[0,4095]` value. (One verifier marked the live-miscompile counterexample *refuted* on this basis; the underlying encoder/pattern weakness is real but inactive.)

**Fix.** Constrain the `LI` brcond patterns (`:654-676`) to a `uimm12` ImmLeaf so they fire only for in-range constants, and harden the encoder with `assert(isUInt<12>(imm))` in the `LI` case so any out-of-range value fails loudly instead of truncating silently.

**Status: FIXED.** Both halves applied: the eight `(LI imm:$b)` brcond patterns are now `uimm12:$b` (so they fire only for `[0,4095]`; out-of-range constants fall back to the sign-correct `ISD::Constant` lowering), and the `LI` case in `SLOW32MCCodeEmitter.cpp` now `assert(isUInt<12>(...))` as a tripwire (active in asserts-enabled builds). Build clean, regression 61/61.

---

## 4. Suggested Fix Order

1. **Floating-point comparison lowering (Findings 1–3)** — fix first, as one coordinated change. These are live, silent, NaN-triggered wrong-code bugs reachable from ordinary C at `-O2` without `-ffast-math`, affecting both f32 and f64. The cleanest uniform remedy is `setCondCodeAction(..., Expand)` for `SETULT/SETULE/SETUGT/SETUGE/SETUEQ/SETONE` on f32/f64, letting LLVM legalize them; the per-pattern `XORI`/`OR` rewrites are the alternative. Re-run the FP regression tests with explicit NaN-operand cases (none currently exist).
2. **`not` pseudo (Finding 4)** — live miscompile of a documented pseudo in any hand-written assembly using `not`. Replace the expansion and fix the spec table; the existing truncation warning is easy to miss in bulk builds.
3. **Interpreter `JALR` (Finding 5)** — one-line reorder; restores agreement with the other three engines and the spec. Cheap and removes the only emulator divergence.
4. **`%hi` on I/S operands (Finding 6)** — small, mirrors the working FMT_U handler; closes a silent whole-word corruption hole for hand-written asm.
5. **Linker synth symbol (Finding 7)** — latent (fires zero times today) but cheap to harden as defense-in-depth alongside the canonical-name fix.
6. **Cost-model and encoder hardening (Findings 8–9)** — non-correctness today; do opportunistically. The `LI` encoder `assert` is worth adding now as a tripwire so the latent pattern weakness cannot silently become a live miscompile later.

Relevant files: `llvm-backend/SLOW32/SLOW32InstrInfo.td`, `llvm-backend/SLOW32/SLOW32ISelLowering.cpp`, `llvm-backend/SLOW32/MCTargetDesc/SLOW32MCCodeEmitter.cpp`, `tools/assembler/slow32asm.c`, `tools/linker/s32-ld.c`, `tools/emulator/slow32.c`.