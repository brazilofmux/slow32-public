# Floating-Point Support in SLOW-32

## Current State

SLOW-32 has extensive floating-point infrastructure already in place across
the ISA, emulators, LLVM backend, and runtime. The self-hosting compiler
(s12cc) does not yet support float types, but nearly everything below it is
ready.

### What works today (via LLVM backend)

- 38 dedicated FP instructions for f32 and f64 arithmetic, comparison,
  conversion, and sign manipulation
- All five emulators execute FP instructions (slow32, slow32-fast,
  slow32-dbt, qemu-system-slow32; s32-emu is the exception)
- LLVM backend compiles C `float` and `double` types, including varargs
- David Gay's `dtoa.c` compiles without modification
- `printf_enhanced.c` supports `%f`, `%g`, `%e`, `%a` via dtoa
- Taylor-series math library (`math_soft.c`) provides all transcendentals
- Hardware wrappers (`math_hw.c`) lower fabs/sqrt to native instructions
- slow32-dbt and QEMU intercept 40+ math libcalls and substitute host libm

### What s12cc needs before it can use floats

- `float` and `double` type keywords
- Float literal parsing (lexer: `3.14`, `1.0e-5`, `.5f`)
- Register pair allocation for f64 (even-register constraint)
- FP instruction emission in codegen
- Implicit int-to-float and float-to-int conversions

---

## ISA Design

SLOW-32 uses a **soft-float-in-GPRs** architecture. There is no separate
FP register file.

- **f32**: One 32-bit GPR holds the IEEE 754 binary32 bit pattern
- **f64**: A register pair `(rN, rN+1)` where `rN` is the low 32 bits and
  `rN+1` is the high 32 bits. The specified register must be **even**.
- **Rounding**: Always round-to-nearest-even (IEEE 754 default)
- **NaN**: Comparisons with NaN return 0. Arithmetic propagates NaN.

### Instruction Summary

| Category       | f32 (opcodes)  | f64 (opcodes)  |
|----------------|:--------------:|:--------------:|
| Arithmetic     | 0x53-0x57 (5)  | 0x61-0x65 (5)  |
| Comparison     | 0x58-0x5A (3)  | 0x66-0x68 (3)  |
| Int32 convert  | 0x5B-0x5E (4)  | 0x69-0x6C (4)  |
| f32/f64 convert| -              | 0x6D-0x6E (2)  |
| Sign manip     | 0x5F-0x60 (2)  | 0x6F-0x70 (2)  |
| Int64 convert  | 0x71-0x74 (4)  | 0x75-0x78 (4)  |
| **Total**      | **18**         | **20**         |

Transcendentals (sin, cos, tan, exp, log, pow, etc.) are library calls,
not instructions.

---

## Calling Convention

### Standard calls

- f32: one register (same as i32)
- f64: **pair-aligned** -- R3:R4, R5:R6, R7:R8, R9:R10
- If a 64-bit value would start at an odd register, it skips to the next
  even pair (wastes one register slot)
- Return: R1 for f32, R1:R2 for f64

### Varargs calls

Pair alignment is incompatible with varargs (gaps in the register save
area). The LLVM backend uses sequential packing instead:

- f64 takes two consecutive registers, no alignment
- Boundary split: if only one register remains, lo half goes in the last
  register, hi half goes to stack offset 0
- Callee save area abuts stack args, so va_arg walks contiguously

This matches the RISC-V ILP32D approach. See `docs/CALLING_CONVENTION.md`
for traced examples including the history of the "gap bug."

---

## Emulator Support

| Emulator        | f32 | f64 | Math interception | Notes |
|-----------------|:---:|:---:|:-----------------:|-------|
| slow32          | yes | yes | no  | Reference interpreter, ~350M inst/sec |
| slow32-fast     | yes | yes | no  | Optimized interpreter |
| slow32-dbt      | yes | yes | yes | JIT; intercepts 40+ libm calls via SYMTAB |
| qemu-system-slow32 | yes | yes | yes | TCG backend; helper.c implements FP ops |
| s32-emu         | no  | no  | no  | Minimal Forth-based emulator |

The DBT and QEMU emulators intercept transcendental function calls by
symbol name and execute them via host libm. This means math-heavy programs
run at near-native speed on these emulators. On slow32 and slow32-fast, the
Taylor-series soft-float library runs in emulated code.

### Intercepted math functions (slow32-dbt)

Unary f32: `sqrtf fabsf sinf cosf tanf expf logf log10f log2f asinf acosf
atanf sinhf coshf tanhf ceilf floorf roundf truncf`

Unary f64: `sqrt fabs sin cos tan exp log log10 log2 asin acos atan sinh
cosh tanh ceil floor round trunc`

Binary f32: `fmodf powf atan2f hypotf copysignf ldexpf modff`

Binary f64: `fmod pow atan2 hypot copysign ldexp modf frexp`

Classification: `isnan isinf isfinite`

---

## Runtime Libraries

### math_hw.c -- Hardware wrappers

Thin builtins that lower directly to native instructions:

```c
double fabs(double x)  { return __builtin_fabs(x);  }  /* FABS.D */
double sqrt(double x)  { return __builtin_sqrt(x);  }  /* FSQRT.D */
float  fabsf(float x)  { return __builtin_fabsf(x); }  /* FABS.S */
float  sqrtf(float x)  { return __builtin_sqrtf(x); }  /* FSQRT.S */
```

### math_soft.c -- Taylor-series transcendentals

Full soft-float implementations of all math.h functions. Uses Taylor series
with range reduction. Benchmarks show Taylor is **2-4.4x faster** than
CORDIC on SLOW-32 due to the 32-cycle hardware MUL (CORDIC needs ~53
iterations of shift-add loops). See `docs/MATH_COMPARISON.md`.

### dtoa.c -- Double-to-string conversion

David Gay's `dtoa()` library for IEEE 754 double-to-string and
string-to-double conversion. Powers the `%f/%g/%e/%a` format specifiers
in `printf_enhanced.c`.

### printf_enhanced.c -- Float-aware printf

Full printf with float formatting via dtoa. Supports `%f`, `%g`, `%e`,
`%a` (hex float) with width and precision modifiers. Uses malloc for
intermediate buffers.

Note: The basic `printf.c` (used in libc_debug.s32a) does **not** support
float formats -- only `%d %u %x %s %c %p`.

---

## LLVM Backend

The LLVM backend for SLOW-32 has complete f32 and f64 support:

- **Register classes**: f32 uses `GPRRegClass`, f64 uses `GPRPairRegClass`
- **Custom lowering**: `BuildPairF64` / `SplitF64` for combining/splitting
  register pairs via stack
- **Data layout**: `f32:32:32-f64:32:32`
- **Calling conventions**: Three variants handle standard calls, varargs
  callee, and varargs caller (with `CC_SLOW32_VarArg_F64` custom handler)
- **i64/f64 conversions**: Custom nodes `FCVT_L`, `FCVT_LU`,
  `FCVT_FROM_L`, `FCVT_FROM_LU`

---

## The Register Pair Problem

Register pairs are the central difficulty for adding float support to s12cc.

f64 values occupy two registers. This affects:

1. **Register allocation**: The allocator must track pairs, enforce
   even-register alignment, and handle the constraint that allocating one
   register in a pair makes the other unavailable.

2. **Spill/reload**: A spill must store 8 bytes (two words). Stack offsets
   for f64 locals need 8-byte alignment or at least pair-aware layout.

3. **Calling convention**: Standard calls use pair-aligned allocation.
   Varargs uses sequential packing. The codegen must handle both.

4. **Interaction with i64**: The same register pair mechanism applies to
   64-bit integers. Getting f64 right likely means getting i64 right at the
   same time (or at least sharing the machinery).

5. **Instruction selection**: f64 arithmetic instructions require even
   register numbers. The BURG instruction selector and register allocator
   must cooperate to satisfy this constraint.

### Possible approaches for s12cc

**Option A: f32 only (no pairs)**

Support `float` as a single-register type, like `int`. No register pair
complexity. This covers a useful subset: single-precision arithmetic,
int-to-float conversion, comparisons. Programs needing `double` would use
explicit `int` pairs with inline assembly or helper functions.

Pros: Minimal compiler changes (TY_FLOAT type + ~15 new instructions in
codegen). Cons: No `double`, can't compile dtoa.c or standard C programs
that default to double promotion.

**Option B: f64 via stack temporaries**

Keep f64 values on the stack, loading into a fixed pair (e.g., r20:r21)
for each operation, storing back immediately. No pair-aware register
allocation needed.

Pros: Simple. Cons: Slow (extra loads/stores around every f64 op).

**Option C: Full pair-aware register allocation**

Extend the linear scan allocator to handle pairs. This is the correct
long-term solution and enables i64 as well.

Pros: Best code quality. Cons: Significant allocator complexity.

---

## Path to Float in s12cc

### Prerequisites (language features needed first)

Before attempting float support, s12cc needs these features solidified:

- [x] Array initializers (`int a[] = {1,2,3}`)
- [x] String array initializers (`char s[] = "hello"`)
- [ ] Long / 64-bit integer type (shares register pair machinery with f64)
- [ ] Wider expression support for complex C idioms in dtoa.c

### Milestone 1: f32 support (no register pairs)

1. Add `TY_FLOAT` to the type system (ast.h)
2. Lex float literals (`3.14f`, `1.0f`) -- store as IEEE 754 bits in i32
3. Parse `float` keyword in declarations
4. Emit f32 instructions: FADD.S, FSUB.S, FMUL.S, FDIV.S, FSQRT.S
5. Emit f32 comparisons: FEQ.S, FLT.S, FLE.S
6. Emit f32 conversions: FCVT.S.W, FCVT.W.S (int <-> float)
7. Implicit int-to-float promotion in mixed expressions

### Milestone 2: f64 support (register pairs)

1. Add `TY_DOUBLE` to the type system
2. Register pair allocation in hir_regalloc.h
3. BuildPairF64 / SplitF64 equivalents for combining i32 halves
4. Emit all f64 instructions with even-register constraint
5. Calling convention: pair-aligned for standard, sequential for varargs
6. Float promotion: f32 -> f64 in mixed expressions (C default)

### Milestone 3: Compile dtoa.c

dtoa.c exercises nearly every corner of C:
- `unsigned long` (i64)
- Extensive pointer arithmetic
- Union type-punning (double <-> uint64_t)
- Complex control flow (goto, nested loops)
- Preprocessor-heavy (#ifdef IEEE_8087, #ifdef Avoid_Coverage, etc.)

Successfully self-hosting dtoa.c would validate the entire float pipeline.

---

## References

- ISA: `docs/INSTRUCTION-SET.md` (lines 151-253)
- Calling convention: `docs/CALLING_CONVENTION.md`
- Math benchmarks: `docs/MATH_COMPARISON.md`
- Runtime math: `runtime/math_hw.c`, `runtime/math_soft.c`
- dtoa: `runtime/dtoa.c`
- Float printf: `runtime/printf_enhanced.c`
- LLVM lowering: `llvm-backend/SLOW32/SLOW32ISelLowering.cpp`
