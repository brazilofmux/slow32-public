# SLOW-32 Emulator Benchmark Results

**Date:** 2026-02-05
**CPU:** AMD Ryzen 5 3600 6-Core Processor
**Method:** Best of 3 runs, sequential execution to avoid interference

---

## Workloads

| Workload | Description | Guest Instructions | Data Size |
|----------|-------------|-------------------|-----------|
| `benchmark_core` | Integer arithmetic, branching, memory access (10M iterations) | 285,018,189 | N/A |
| `validatecsv_ragel` | Ragel -G2 goto-driven CSV validator (all csv2/ files) | 1,108,462,788 | 103.6 MB |

---

## Emulators Under Test

| Emulator | Description |
|----------|-------------|
| `slow32` | C++ interpreter (baseline) |
| `slow32-fast` | Optimized C interpreter with pre-decoded instructions |
| `slow32-dbt -4` | JIT dynamic binary translator, Stage 4 (superblocks + register cache + peephole) |
| `slow32-dbt -4 -U` | Same as above, bounds/W^X checks disabled |
| `QEMU TCG` | QEMU Tiny Code Generator backend for SLOW-32 |
| Native x86-64 | GCC -O2, Ragel -G2, mmap or fread (validatecsv only) |

---

## Results: benchmark_core

| Emulator | Time (s) | MIPS | Speedup vs interpreter |
|----------|----------|------|------------------------|
| slow32 (interpreter) | 1.907 | 149 | 1.0x |
| slow32-fast | 0.751 | 380 | 2.5x |
| QEMU TCG | 0.246 | 1,159 | 7.8x |
| **slow32-dbt -4 (safe)** | **0.048** | **5,938** | **39.7x** |
| **slow32-dbt -4 -U (unsafe)** | **0.047** | **6,064** | **40.6x** |

### Key Ratios (benchmark_core)

- DBT is **5.1x faster** than QEMU TCG
- DBT safe vs unsafe: virtually identical (2% difference — bounds checks are cheap)

---

## Results: validatecsv_ragel (103.6 MB CSV)

| Emulator | Time (s) | MIPS | MB/s | Speedup vs interpreter |
|----------|----------|------|------|------------------------|
| slow32 (interpreter) | 9.802 | 113 | 10.6 | 1.0x |
| slow32-fast | 3.479 | 319 | 29.8 | 2.8x |
| QEMU TCG | 0.952 | 1,164 | 108.9 | 10.3x |
| **slow32-dbt -4 (safe)** | **0.318** | **3,486** | **326** | **30.8x** |
| **slow32-dbt -4 -U (unsafe)** | **0.295** | **3,758** | **351** | **33.2x** |
| Native x86-64 (fread) | 0.202 | — | 513 | — |
| Native x86-64 (mmap) | 0.144 | — | 720 | — |

### Key Ratios (validatecsv)

- DBT is **3.0x faster** than QEMU TCG
- DBT safe vs unsafe: 7.8% difference (more I/O-bound, checks matter slightly more)
- DBT (unsafe) is only **2.05x slower** than native mmap
- DBT (unsafe) is only **1.46x slower** than native fread (apples-to-apples I/O model)
- DBT processes CSV at **351 MB/s** — within 2x of native

---

## Summary Table (All Results)

### Wall-Clock Time (seconds, best of 3)

| Emulator | benchmark_core | validatecsv |
|----------|---------------|-------------|
| slow32 (interpreter) | 1.907 | 9.802 |
| slow32-fast | 0.751 | 3.479 |
| QEMU TCG | 0.246 | 0.952 |
| slow32-dbt -4 (safe) | 0.048 | 0.318 |
| slow32-dbt -4 -U (unsafe) | 0.047 | 0.295 |
| Native x86-64 (fread) | — | 0.202 |
| Native x86-64 (mmap) | — | 0.144 |

### MIPS (millions of guest instructions per second)

| Emulator | benchmark_core | validatecsv |
|----------|---------------|-------------|
| slow32 (interpreter) | 149 | 113 |
| slow32-fast | 380 | 319 |
| QEMU TCG | 1,159 | 1,164 |
| slow32-dbt -4 (safe) | 5,938 | 3,486 |
| slow32-dbt -4 -U (unsafe) | 6,064 | 3,758 |

### Throughput (MB/s, validatecsv only)

| Emulator | MB/s |
|----------|------|
| slow32 (interpreter) | 10.6 |
| slow32-fast | 29.8 |
| QEMU TCG | 108.9 |
| slow32-dbt -4 (safe) | 326 |
| slow32-dbt -4 -U (unsafe) | 351 |
| Native x86-64 (fread) | 513 |
| Native x86-64 (mmap) | 720 |

---

## Observations

1. **The DBT is extraordinarily fast.** At ~6 GIPS on compute-bound code, it approaches

   native execution speed on a 3.6 GHz processor. The JIT-compiled superblocks with
   register caching and peephole optimization produce highly efficient x86-64 code.

2. **QEMU TCG is respectable but not competitive.** At ~1.16 GIPS it's a solid general-purpose

   emulator, but the custom DBT is 3-5x faster due to architecture-specific optimizations.

3. **Bounds checks are nearly free.** The safe vs unsafe DBT difference is only 2-8%,

   meaning the W^X and bounds checking overhead is negligible.

4. **I/O-bound workloads narrow the gap.** validatecsv shows lower MIPS across all emulators

   (3,486 vs 5,938 for DBT) because time is spent in emulated file I/O syscalls, not just
   compute. Even so, the DBT processes CSV data at 326-351 MB/s.

5. **Within striking distance of native.** The DBT running a SLOW-32 binary processes CSV

   at 351 MB/s vs native fread at 513 MB/s — the emulated program runs at **68% of native
   speed** on a real-world I/O workload.

---

## Raw Timing Data

```
benchmark_core.s32x (3 runs each):

  slow32 (interpreter):     1.917  1.920  1.907  → best 1.907s
  slow32-fast:              0.753  0.751  0.757  → best 0.751s
  slow32-dbt -4 (safe):    0.048  0.048  0.049  → best 0.048s
  slow32-dbt -4 -U:        0.047  0.049  0.047  → best 0.047s
  QEMU TCG:                0.647  0.246  0.250  → best 0.246s

validatecsv_ragel.s32x + csv2/*.csv (3 runs each):

  slow32 (interpreter):     9.802  9.908  9.892  → best 9.802s
  slow32-fast:              5.020  3.765  3.479  → best 3.479s
  slow32-dbt -4 (safe):    0.318  0.336  0.326  → best 0.318s
  slow32-dbt -4 -U:        0.295  0.297  0.296  → best 0.295s
  QEMU TCG:                0.952  0.952  0.952  → best 0.952s
  Native x86-64 (mmap):    0.145  0.144  0.146  → best 0.144s
  Native x86-64 (fread):   0.202  0.205  0.203  → best 0.202s
```
