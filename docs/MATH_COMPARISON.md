# Taylor Series vs CORDIC on SLOW-32

## Background

SLOW-32 has a 32-cycle hardware MUL instruction but no hardware FPU. All
floating-point math runs in software. The initial hypothesis was that CORDIC
(which uses only shifts and adds) would outperform Taylor series (which
relies heavily on multiplication) on this architecture.

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
cycle count** (which includes the 32-cycle MUL penalty).

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

This ignores iteration count. Double-precision CORDIC needs **53
iterations** (one per mantissa bit) of a multi-instruction loop body
(shift, compare, conditional add/subtract, for each of x, y, and z).
That's ~950-1300 instructions per call.

Taylor series for sin/cos converges in **~15 terms** with range reduction
to [-pi, pi]. Each term is one multiply and one add. Even at 32
cycles/multiply, 15 iterations * ~18 instructions ≈ 270 instructions,
with ~870 cycles. The 32-cycle MUL is expensive per-instruction, but
there aren't enough of them to overcome CORDIC's iteration overhead.

The break-even point would require MUL to cost roughly **100+ cycles**
before CORDIC becomes competitive for double precision. For single
precision (24 iterations), CORDIC would be closer but still likely
slower than a well-tuned Taylor with early termination.

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
providing CORDIC. Run on `slow32-fast` emulator with cycle-accurate MUL
timing (32 cycles). Each function called 1000 times in isolation to get
stable per-call instruction and cycle counts.

Reference values for accuracy tests are IEEE double constants computed
on x86-64 host.
