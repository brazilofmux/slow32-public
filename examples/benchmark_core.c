// Simple CPU-bound benchmark kernels for SLOW-32.
//
// Build:
//   ./slow32cc examples/benchmark_core.c -O2 -o benchmark_core.s32x
//   # Override iteration count if desired
//   ./slow32cc examples/benchmark_core.c -O2 -DBENCH_ITERS=2000000 -o benchmark_core.s32x
//
// Run and time:
//   time ./tools/emulator/slow32 benchmark_core.s32x >/dev/null
//   time ./tools/emulator/slow32-fast benchmark_core.s32x >/dev/null
//
// The program prints a summary and a checksum so the optimizer cannot
// eliminate the work. When benchmarking, redirect stdout/stderr to avoid
// skew from the host I/O path.

#include <stdint.h>
#include <stdio.h>

#ifndef BENCH_ITERS
#define BENCH_ITERS 10000000u
#endif

#define NOINLINE __attribute__((noinline))

static uint32_t NOINLINE bench_arith(uint32_t iterations) {
    uint32_t acc = 0x12345678u;
    uint32_t x = 0x9e3779b9u;

    for (uint32_t i = 0; i < iterations; ++i) {
        acc += x;
        acc ^= acc << 7;
        acc ^= acc >> 5;
        x += 0x3c6ef372u;
    }

    return acc ^ x;
}

static uint32_t NOINLINE bench_branch(uint32_t iterations) {
    uint32_t acc = 0;
    uint32_t toggle = 0;

    for (uint32_t i = 0; i < iterations; ++i) {
        if ((toggle & 1u) == 0) {
            acc += i ^ (toggle << 3);
        } else {
            acc ^= i + (toggle << 1);
        }

        if ((acc & 0x10u) == 0) {
            acc = (acc << 1) | 1u;
        }

        toggle = (toggle + 3u) & 0x7u;
    }

    return acc ^ toggle;
}

static uint32_t NOINLINE bench_mem(uint32_t iterations) {
    uint32_t buf[64];
    for (uint32_t i = 0; i < 64; ++i) {
        buf[i] = i * 0x01010101u;
    }

    uint32_t acc = 0;
    uint32_t idx = 0;

    for (uint32_t i = 0; i < iterations; ++i) {
        idx = (idx + 7u) & 63u;
        acc ^= buf[idx] + i;
        buf[idx] = acc ^ (i << 4);
    }

    for (uint32_t i = 0; i < 64; ++i) {
        acc ^= buf[i];
    }

    return acc;
}

int main(void) {
    const uint32_t arith_iters = BENCH_ITERS;
    const uint32_t branch_iters = BENCH_ITERS;
    const uint32_t mem_iters = (BENCH_ITERS / 4u) + 1u;

    uint32_t arith = bench_arith(arith_iters);
    uint32_t branch = bench_branch(branch_iters);
    uint32_t mem = bench_mem(mem_iters);

    uint32_t checksum = arith ^ branch ^ mem ^ BENCH_ITERS;

    printf("BENCH iters=%u mem_iters=%u\n", BENCH_ITERS, mem_iters);
    printf("  arith : 0x%x\n", arith);
    printf("  branch: 0x%x\n", branch);
    printf("  mem   : 0x%x\n", mem);
    printf("checksum: 0x%x\n", checksum);

    // Non-zero exit code still keeps the program in-band while ensuring
    // the result depends on the benchmark.
    return (int)(checksum & 0xFF);
}
