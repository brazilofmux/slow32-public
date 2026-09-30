# Taylor Series vs CORDIC on SLOW-32

## Background

SLOW-32 has had IEEE 754 f32 and f64 instructions since 2026-02-01
(f432799d): FADD/FSUB/FMUL/FDIV/FSQRT in both widths, plus compares and
conversions. `math_soft.c` is "soft" only in the sense that it is C rather
than a single instruction; it compiles to those FP instructions. The integer
MUL/MULH/MULHU cost 32 cycles in `slow32-fast`'s cycle model and DIV/REM 64;
every FP instruction costs one.

An earlier version of this document, written on 2026-02-09, said SLOW-32 had
no hardware FPU and that all floating-point math ran in software. That was
already wrong when it was written, and the reasoning below has been corrected
to match. The measurements were taken with the FP instructions in place.

The initial hypothesis was that CORDIC (which uses only shifts and adds)
would outperform Taylor series (which relies heavily on multiplication) on
this architecture.

This turned out to be wrong.

## Results

### Performance (1000 calls each, input in typical range)

| Function   | Taylor insns | CORDIC insns | Taylor cycles | CORDIC cycles |
|------------|-------------:|-------------:|--------------:|--------------:|
| sin(0.7)   |          277 |          950 |           870 |         1,705 |
| cos(0.7)   |          286 |          950 |           885 |         1,705 |
| atan2(1,1) |          270 |        1,014 |           398 |         1,766 |
| exp(2.0)   |          460 |        1,332 |           630 |         1,820 |

Taylor is **2-3.4x faster by instruction count** and **2-4.4x faster by
cycle count**. The cycle counts include the 32-cycle integer MUL, but for
Taylor that penalty comes from the loop's integer denominator
`2*i*(2*i+1)`, one MUL per term (15 x 31 = 465 of sin's ~590 extra
cycles), not from the floating-point multiplies, which cost one cycle.

### Code Size

| Metric          | Taylor  | CORDIC  |
|-----------------|--------:|--------:|
| Object file     | 4,992 B | 7,156 B |
| Assembly lines  |     754 |   1,108 |

Taylor is **30% smaller**.

### Accuracy (absolute error for trig, relative error for exp)

| Function | Taylor max err | CORDIC max err |
|----------|---------------:|---------------:|
| sin      |       1.90e-11 |       1.57e-11 |
| cos      |       4.33e-11 |       4.75e-11 |
| atan2    |       3.33e-11 |       2.69e-11 |
| exp      |       6.56e-07 |       5.84e-07 |

Both achieve ~10-11 digits for trig and ~6-7 digits for exp. The
precision is comparable and limited by IEEE double range reduction
quality, not the core algorithm.

## Why CORDIC Lost

The original reasoning was:

> MUL takes 32 cycles on SLOW-32. CORDIC replaces multiplications with
> shifts and adds (1 cycle each). Therefore CORDIC should be faster.

Its premise was wrong twice over:

- **The multiplies in question are FP, and they cost one cycle.** The
  32-cycle MUL is the integer instruction. Taylor's floating-point
  multiplies and divides are single FMUL.D/FDIV.D instructions.
- **This CORDIC does not shift.** `math_cordic.c` iterates in doubles, so
  each "shift" is an FMUL.D by a halving `p2`. Every one of its 52
  iterations does two FP multiplies plus a third to halve `p2`, plus a
  compare, a table load and three add/subtracts: more FP multiplication
  than the Taylor series it was meant to avoid.

So the contest came down to iteration count. Double-precision CORDIC
needs **52 iterations** (about one per mantissa bit); that is ~950-1300
instructions per call. The Taylor series for sin/cos runs **15 terms**
after range reduction to [-pi, pi], at ~18 instructions a term: ~270
instructions.

An integer, fixed-point CORDIC with real shifts would avoid the FP
multiplies, but it would still need ~52 iterations against Taylor's 15
terms, plus conversions in and out of fixed point, against one-cycle
FMUL.D. That is not a race it can win on this ISA. For single precision
(24 iterations) it would be closer, but still likely slower than a
well-tuned Taylor with early termination.

## When CORDIC Would Win

- **No hardware multiplier at all** (bit-serial multiply in software)
- **Fixed-point only** (no IEEE double overhead)
- **Hardware CORDIC** (barrel shifter + dedicated rotation unit)
- **Multiple outputs needed** (sin and cos simultaneously from one rotation)
- **Very low precision** (8-16 bit, where iteration count is small)

Classic 8-bit systems (6502, Z80) hit several of these: no MUL
instruction, fixed-point arithmetic, and low precision targets. That's
where CORDIC earned its reputation.

## Recommendation

**Taylor series should be the default** for SLOW-32's software math
library. It is faster, smaller, and equally accurate.

CORDIC remains available in `math_cordic.c` for reference and for any
future use case where simultaneous sin/cos or hardware-assisted rotation
is needed.

## Test Methodology

Benchmark: `examples/math_shootout.c` with `examples/math_taylor.c`
providing explicit Taylor implementations and `runtime/math_cordic.c`
providing CORDIC. Run on the `slow32-fast` emulator, whose cycle model
charges 32 cycles for integer MUL/MULH/MULHU, 64 for DIV/REM, and one
for each FP instruction. Each function called 1000 times in isolation to get
stable per-call instruction and cycle counts.

Reference values for accuracy tests are IEEE double constants computed
on x86-64 host.
