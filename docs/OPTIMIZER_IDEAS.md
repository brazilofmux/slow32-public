# DBT Peephole Optimizer Ideas

## Status: Active Development

Last updated after inline f32 FP SSE commit (2026-02-04).

## Completed

- [x] **Inline f32 FP ops** — 14 opcodes now use SSE instead of helper call
- [x] **Pattern 0**: `xor r,r; cmp r,r` → `xor r,r; test other,other`
- [x] **Pattern 0b**: `mov r,imm; mov [rbp+d],r; mov r,imm` → `mov [rbp+d],imm`
- [x] **Pattern 0c**: `mov r,0; mov [rbp+d],r; mov r,0; mov [rbp+d],r` → remove dup zero
- [x] **Pattern A**: `load r,[rbp+d]; store [rbp+d],r` → remove dead store-back
- [x] **Pattern B**: `store [rbp+d],r; load r,[rbp+d]` → remove redundant reload
- [x] **Pattern C**: `mov r,imm; mov r,imm` → remove dead first mov

## High Priority — Patterns Observed in Hot Blocks

### 1. Conditional-over-unconditional jump folding
**Pattern**: `ja/jae/jb/jbe/je/jne +5; jmp rel32` → invert to single `jbe/jb/jae/ja/jne/je rel32`
**Observed**: Every memory bounds check emits this. In the CSV validator's hottest
block (0x1300, 2085 bytes), this pattern occurs ~20 times.
**Savings**: 5 bytes per occurrence (11 → 6 bytes)
**Impact**: ~100 bytes per hot block, ~5% code size reduction

### 2. Store-then-store to same address
**Pattern**: `mov [rbp+d],imm1; mov [rbp+d],imm2` → remove first store
**Observed**: At 0x18b-0x192 in CSV hot block: `movl $0x200000, 0x14(%rbp); movl $0x2000b4, 0x14(%rbp)`
**Savings**: 7-10 bytes per occurrence
**Note**: Generalizes pattern 0c. Need both imm-to-mem and reg-to-mem forms.

### 3. Redundant mov before cmp
**Pattern**: `mov rA,rB; mov rC,rD; cmp rC,rA` → `cmp rD,rB`
**Observed**: At 0x79-0x7d in benchmark hot block and many comparison sequences
**Savings**: 4 bytes per occurrence (two 2-byte movs eliminated)
**Complexity**: Medium — need to verify rA and rC are dead after the cmp

### 4. Dead first mov (REX-prefixed)
**Pattern**: `[REX] mov r,imm; [REX] mov r,imm` (same dest) → remove first
**Observed**: At 0x255-0x25b: `mov $0x200000,%r12d; mov $0x2000c0,%r12d`
**Note**: Pattern C already handles non-REX form. Need to extend for REX prefix.
**Status**: Pattern C already handles this via REX consumption. VERIFY.

## Medium Priority

### 5. NOP compaction
After peephole replaces instructions with NOPs, a final pass could compact them
out and adjust relative jumps. Currently NOPs waste icache space.
**Complexity**: High — requires relocation of all relative offsets
**Impact**: Recovers whatever the other patterns save

### 6. Redundant loads from same guest register
**Pattern**: `mov rA,[rbp+d]; ... mov rB,[rbp+d]` (same d, no intervening store to d)
**Savings**: 3 bytes per occurrence
**Complexity**: High — needs data-flow tracking
**Note**: Register cache already handles the common case

## Low Priority / Future

### 7. Fuse mov+op into op-with-memory-operand
**Pattern**: `mov rA,[rbp+d]; add rB,rA` → `add rB,[rbp+d]`
**Savings**: ~1 byte (trade mov+add for add-mem)
**Risk**: May be slower on some µarchs due to load-op fusion

### 8. Replace `mov r,0` with `xor r,r`
**Pattern**: `mov r32, 0` (5 bytes) → `xor r32,r32` (2 bytes)
**Savings**: 3 bytes per occurrence
**Note**: Changes flags — only safe if flags aren't live

## Non-Peephole Optimization Ideas

### Register cache improvements
- Profile which guest regs are hottest per block and prioritize those
- Spill less aggressively at block exits that chain to known targets

### Memory access optimization
- Constant-fold bounds checks when address is a known constant
- Hoist repeated bounds checks out of inner loops (would need loop detection)

### Branch optimization
- Convert `jcc rel32` to `jcc rel8` when target is within ±127 bytes
- This is a post-peephole pass since peephole may shorten the code
