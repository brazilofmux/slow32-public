# CSV Parser Heisenbug - LLVM Codegen Issue

## STATUS: FIXED (2026-01-19)

The bug has been resolved by splitting JAL/JALR into separate opcodes for calls vs jumps,
allowing proper call-clobber modeling without affecting regular branch semantics.

**Verification:** regal stats now works correctly without the memory barrier workaround:
- 51356 lines loaded
- 22764 transactions
- Books balanced

---

## Original Summary (for reference)

The CSV parser in the regal accounting application exhibited a Heisenbug where the state machine failed at `-O2` without a memory barrier workaround. The simple standalone test (`test_csv_nodebug.c`) did not reproduce the issue - it only manifested in the full regal build context.

## Files in this directory

- `csv.c` - Source file (without the barrier - this is the broken version)
- `csv.h` - Header file (if present)
- `csv-with-barrier.ll` - LLVM IR with memory barrier (WORKING)
- `csv-with-barrier.s` - Assembly with memory barrier (WORKING)
- `csv-no-barrier.ll` - LLVM IR without memory barrier (BROKEN)
- `csv-no-barrier.s` - Assembly without memory barrier (BROKEN)

## The Problem

In `csv_parse()`, the main parsing loop has this structure:

```c
for (size_t i = 0; i < p->len; i++) {
    unsigned char ch = (unsigned char)p->data[i];

    // Without this barrier, parsing fails silently
    // __asm__ volatile("" : : : "memory");

    switch (p->state) {
        case CSV_STATE_MAIN:
            // ... state transitions ...
    }
}
```

### Working IR (with barrier)
```llvm
21:
  %22 = phi i32 [ 0, %10 ], [ %332, %331 ]
  %23 = load ptr, ptr %0, align 4
  %24 = getelementptr inbounds nuw i8, ptr %23, i32 %22
  %25 = load i8, ptr %24, align 1           ; Load character
  tail call void asm sideeffect "", "~{memory}"()  ; BARRIER
  %26 = load i32, ptr %11, align 4          ; Load state
  switch i32 %26, label %377 [...]
```

### Broken IR (without barrier)
```llvm
21:
  %22 = phi i32 [ 0, %10 ], [ %346, %345 ]
  %23 = load ptr, ptr %0, align 4
  %24 = getelementptr inbounds nuw i8, ptr %23, i32 %22
  %25 = load i8, ptr %24, align 1           ; Load character
  %26 = load i32, ptr %11, align 4          ; Load state (NO BARRIER)
  switch i32 %26, label %391 [...]
```

## Symptoms

- With barrier: All CSV files load correctly (51356 lines from lines.csv)
- Without barrier: All CSV files fail to load (0 lines)
- Adding `printf()` debug statements makes the code work (classic Heisenbug)
- The issue only manifests at `-O2`; cannot test `-O1` as that historically crashed LLC

## Assembly Diff Stats

- 966 lines differ between working and broken `.s` files
- 1453 lines differ between working and broken `.ll` files
- The barrier prevents some optimization that corrupts the state machine logic

## Reproduction

```bash
# From /ztank/secret/sdennis/regal
cd src/regal

# Build without barrier (broken)
# Edit csv.c to remove the __asm__ volatile line
./build-slow32.sh

# Test
cd /ztank/secret/sdennis/regal
~/slow-32/tools/emulator/slow32 ./src/regal/regal.s32x stats
# Result: Companies: 0 / 4, Accounts: 0 / 512, etc.

# Build with barrier (working)
# Edit csv.c to restore the __asm__ volatile line
./build-slow32.sh

# Test again
~/slow-32/tools/emulator/slow32 ./src/regal/regal.s32x stats
# Result: Companies: 2 / 4, Accounts: 438 / 512, etc.
```

## Why standalone test doesn't reproduce it

The standalone `test_csv_nodebug.c` works correctly even without the barrier. This suggests the bug is triggered by:

1. Interaction with other code in the full regal build
2. Specific memory layout or alignment in the larger executable
3. Link-time optimization effects across multiple compilation units
4. The 50MB+ BSS section affecting code generation or register allocation

## Investigation hints

The barrier forces the compiler to:
1. Not hoist the state load out of the loop
2. Not cache values across iterations
3. Reload memory after each character is processed

Something in the optimization pipeline is incorrectly assuming loop-invariant behavior for values that actually change each iteration.

## Additional Analysis

### Back-edge Analysis

In the broken version, there are many jumps to `.LBB3_5` (loop increment):
```
jal r0, .LBB3_5  (multiple occurrences)
```

But only one jump to `.LBB3_6` (state reload). The control flow is:

```
.LBB3_5:  i++ ; check bounds
          |
          v (fall through if not done)
.LBB3_6:  load state
          |
          v
.LBB3_7:  load char ; switch
```

So jumping to .LBB3_5 should still reload state at .LBB3_6. However, the optimizer 
may have made assumptions about register liveness that don't hold.

### Possible Root Causes

1. **Register allocation bug**: r1 is used for both state and intermediate values.
   The optimizer may assume r1 holds state when it actually holds something else.

2. **Loop-invariant code motion**: The optimizer might be treating p->state as 
   loop-invariant in some code paths, even though it changes each iteration.

3. **Phi node miscompilation**: The phi nodes at the loop header may not be 
   correctly merging all incoming values for the state.

4. **Dead code elimination**: Some state stores might be incorrectly eliminated.

### Files for Investigation

Run llc with `-print-after-all` to see which optimization pass introduces the bug:
```bash
llc -mtriple=slow32-unknown-none -print-after-all csv-no-barrier.ll 2>&1 | less
```

Compare with:
```bash
llc -mtriple=slow32-unknown-none -print-after-all csv-with-barrier.ll 2>&1 | less
```

## ROOT CAUSE IDENTIFIED

The bug is **missing call-clobber modeling** in the SLOW-32 LLVM backend, not MachineSinking.

### Evidence

1. `SLOW32GenRegisterInfoTargetDesc.inc` shows `getRegMasks()` returns `{}` - no call-preserved mask
2. `SLOW32ISelDAGToDAG.cpp` custom CALL selector drops the regmask operand entirely

### The Bug

After the callback (`jalr lr, r12, 0`), the generated code uses r3 directly without reloading:
```asm
    jalr lr, r12, 0      ; Call callback (emit_row)
.LBB3_39:
    bgtu r3, r28, ...    ; Uses r3 - BUT CALLBACK CLOBBERED IT!
```

The optimizer doesn't know r3 is caller-saved, so it assumes the value survives the call.

### The Fix (being implemented)

1. Define callee-saved register masks in TableGen:
   ```tablegen
   def CSR_SLOW32 : CalleeSavedRegs<(add R11, R12, ... R28, R30, R31)>;
   ```

2. Preserve the regmask operand when selecting CALL in `SLOW32ISelDAGToDAG.cpp`

### Why the Memory Barrier Works

The barrier forces a reload on the next loop iteration, hiding the fact that r3 was clobbered.
Without it, stale register values get used after function calls.

---

## Fix Attempts (as of 2026-01-19)

### Fix #1: Add regmask to CALL instructions
- Added `CSR_SLOW32_RegMask` in TableGen
- `getRegMasks()` now returns the mask
- Regmask preserved in CALL selection
- **Result**: Regmask is present, r3 NOT in callee-saved list (correctly marked as clobbered)
- **Status**: PARTIAL - regmask generated but bug persists

### Fix #2: Strip explicit argument operands from expanded CALL
- When expanding `JAL_CALL`/`JALR_CALL` pseudo-instructions to real `JAL`/`JALR`
- Drop explicit argument registers ($r3/$r4/$r5/$r6)
- Keep only: target/rs1, regmask, implicit defs/uses
- **Result**: Bug still persists
- **Status**: INCOMPLETE

### Current State of the Bug

After both fixes, the assembly still shows r3 used without reload:

```asm
    ldw r3, r11+12       ; Load p->line_number into r3
    beq r12, r27, .LBB3_4
.LBB3_38:
    add r5, r14, r0      ; Set up callback args
    add r6, r13, r0
    jalr lr, r12, 0      ; CALL emit_row - SHOULD CLOBBER r3
.LBB3_39:
    bgtu r3, r28, ...    ; USES r3 WITHOUT RELOAD!
.LBB3_40:
    slli r1, r3, 2       ; Uses r3 for switch dispatch
```

### Key Observation

In this case, r3 does NOT hold a function argument - it holds `p->line_number` which was loaded
BEFORE the call for a different purpose. The explicit operand stripping fixed argument registers,
but this is a general-purpose register being used across a call.

The regmask should tell the register allocator "r3 is dead after this call", but something
is still keeping it artificially live. Possibilities:

1. The regmask isn't being consulted during register allocation for non-argument uses
2. There's still something keeping r3 live (implicit use? liveness analysis bug?)
3. The spill/reload logic isn't being triggered for values live across calls

### Debugging Suggestions

1. Check if `-verify-machineinstrs` passes now (it was failing before)
2. Use `-print-after=regalloc` to see if r3 is marked as live across the call
3. Check if there's an implicit-use of r3 on the JALR instruction keeping it live
4. Compare MIR output between barrier and no-barrier versions at regalloc stage
