# SLOW-32 Calling Convention

## Registers

| Register | Role | Saved by |
|----------|------|----------|
| R0 | Hardwired zero | N/A |
| R1-R2 | Return values (R1:R2 for 64-bit) | Caller |
| R3-R10 | Arguments | Caller |
| R11-R28 | General purpose | Callee |
| R29 | Stack pointer (SP) | Callee |
| R30 | Frame pointer (FP) | Callee |
| R31 | Link register (LR) | Callee |

## Standard Calls (non-varargs)

Arguments are assigned left-to-right from R3-R10:

- **i32, f32**: one register each
- **i64, f64**: pair-aligned — R3:R4, R5:R6, R7:R8, R9:R10
- Overflow goes to the stack, 4-byte aligned

Pair alignment means a 64-bit value that would start at an odd register
(e.g. R4) skips to the next even pair (R5:R6). This wastes a register but
is simple and matches common RISC conventions.

Return values use R1 (and R1:R2 for 64-bit).

## Varargs Calls

Varargs has a harder constraint: the callee has **no type information**
about the variadic arguments. It saves all arg registers into a contiguous
"register save area" on the stack, and `va_arg` walks this area
byte-by-byte. The save area must abut the caller's stack-passed arguments
with no gaps, forming a single contiguous data stream.

This means pair-aligned allocation is wrong for varargs — any skipped
register becomes a garbage slot in the save area.

### Allocation Rules

All varargs arguments pack sequentially into R3-R10, left-to-right, with
no alignment gaps:

- **i32, f32**: one register
- **i64**: type legalization splits this to 2x i32 before CC analysis,
  so it naturally takes two consecutive registers with no special handling
- **f64**: a custom CC handler (`CC_SLOW32_VarArg_F64`) allocates two
  consecutive individual registers. If only one register remains, the f64
  **splits**: lo half in the last register, hi half at the start of the
  stack overflow area

After the format string in R3, this gives 7 vararg register slots
(R4-R10) — enough for 3 doubles plus an i32, or 7 i32s, etc.

### Callee Behavior

The callee (e.g. printf):
1. Processes fixed params (fmt string) via `CC_SLOW32_VarArg`
2. Saves all remaining arg registers (R4-R10 typically) into a
   fixed-size save area at negative offsets from FP
3. Sets `VarArgsFrameIndex` to the start of this save area
4. `va_arg` reads sequentially from the save area, then continues
   into the caller's stack arguments

### The Register/Stack Boundary Split

When an f64 straddles the register/stack boundary (e.g. R10 is the last
available register), the caller:
- Puts the lo 32 bits in R10
- Puts the hi 32 bits at stack offset 0

The callee saves R10 at the end of its save area. Since the save area is
at negative FP offsets and stack args are at non-negative FP offsets, the
data is contiguous: `[...save area..., R10_value | stack_hi_half, ...]`.

`va_arg` reads 8 bytes starting at the R10 slot, crossing the save
area/stack boundary, and gets the correct f64.

### Traced Example

`printf("B: %s %f %f %f %f\n", "X", 1.0, 2.0, 3.0, 4.0)`:

```
R3  = fmt pointer     (fixed param)
R4  = "X" pointer     (i32)
R5  = 1.0 lo          (f64, both halves fit)
R6  = 1.0 hi
R7  = 2.0 lo          (f64, both halves fit)
R8  = 2.0 hi
R9  = 3.0 lo          (f64, both halves fit)
R10 = 3.0 hi
stack[0..3] = 4.0 lo  (f64, entirely on stack)
stack[4..7] = 4.0 hi
```

Callee saves R4-R10 → 7 slots, all meaningful. va_arg walks:
- "X" (4B) — R4
- 1.0 (8B) — R5:R6
- 2.0 (8B) — R7:R8
- 3.0 (8B) — R9:R10
- 4.0 (8B) — stack

With a boundary split (e.g. if "X" and "Y" preceded three doubles):
```
R3  = fmt, R4 = "X", R5 = "Y"
R6  = 1.0 lo, R7 = 1.0 hi
R8  = 2.0 lo, R9 = 2.0 hi
R10 = 3.0 lo          ← split: lo in register
stack[0..3] = 3.0 hi  ← hi on stack
stack[4..7] = ...
```

## History and Lessons Learned

### The Gap Bug (and why it appeared twice)

The original varargs CC used pair-based `CCAssignToRegWithShadow` for f64,
which allocates from a list of (lo, shadow) pairs. When preceding i32 args
shift the starting register to an odd position, the last register in the
set can't start a new pair, so the f64 goes entirely to stack — leaving
that last register as an uninitialized gap in the save area.

**First appearance**: With R3-R10 (8 registers), `printf` with 4 doubles
would fill R4:R5, R6:R7, R8:R9, then the 4th f64 couldn't fit (only R10
left), so R10 was a gap.

**"Fix" (commit 444bab45)**: Restricted varargs to R3-R7 (5 registers).
This reduced the surface — with fewer registers, the gap appeared less
often. But it didn't fix the root cause.

**Second appearance**: `printf("B: %s %f %f %f %f", "X", 1.0, 2.0, 3.0,
4.0)` consumed R3 (fmt) and R4 ("X"), leaving R5-R7. The f64 pair R5:R6
fit, but the next f64 found only R7 — gap again, same bug at a different
boundary.

### The Actual Fix

The root cause is that **pair-based allocation is incompatible with
varargs** whenever argument sizes are heterogeneous. The fix is sequential
individual allocation with boundary splitting — the same approach RISC-V
uses for its ILP32D ABI (RV32 with hardware doubles, where f64 args in
varargs are passed in integer register pairs without alignment).

With the root cause fixed, the register restriction was unnecessary, so
varargs was restored to the full R3-R10 set.

### Key Insight

The "odd register count" problem and the "mixed-size gap" problem are the
same problem: pair-based allocation creates gaps at any boundary where the
number of remaining registers is odd. The unified solution — sequential
packing with boundary splitting — works regardless of register count or
argument type mix.

## LLVM Implementation Notes

Three calling conventions in `SLOW32CallingConv.td`:

- **`CC_SLOW32`**: Standard calls. Pair-aligned f64/i64 (R3:R4, R5:R6,
  R7:R8, R9:R10). Used for both caller and callee sides.
- **`CC_SLOW32_VarArg`**: Callee-side varargs. Only processes fixed
  params. Uses consecutive-pair shadow for f64 (since fixed params have
  known types, pair allocation is safe here).
- **`CC_SLOW32_VarArgCall`**: Caller-side varargs. Uses
  `CCCustom<"CC_SLOW32_VarArg_F64">` for f64 sequential packing. i32/f32
  use individual registers. i64 rules are absent because type legalization
  splits i64 to 2x i32 before CC analysis (dead code was removed).

The custom handler `CC_SLOW32_VarArg_F64` in `SLOW32ISelLowering.cpp`:
- Allocates two registers individually from R3-R10
- Both available → normal `getReg` assignment
- Only one available → `getCustomReg` + `getCustomMem` (split)
- None available → returns false, falls through to `CCAssignToStack`

`LowerCall` handles the split case: lo half goes to a register via
`RegsToPass`, hi half becomes an explicit stack store.
