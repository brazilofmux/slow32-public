# Examples & Benchmarks

- `benchmark_core.c` — hot-loop microbenchmark for comparing `slow32`
  and `slow32-fast` throughput. Build with `./slow32cc examples/benchmark_core.c -O2 -o benchmark_core.s32x`
  (override `BENCH_ITERS` if you need a longer run). Time each emulator
  with stdout redirected, e.g. `time ./tools/emulator/slow32-fast benchmark_core.s32x >/dev/null`.

- Other source files are small sanity programs that exercise pieces of the
  toolchain (hello world variants, arithmetic tests, etc.). Use them as
  references when wiring up new inputs.

- `validatejson.c` — strict JSON validator (RFC 8259-focused) with UTF-8
  and escape validation. Build with:
  `./slow32cc --libc=mmio -O2 examples/validatejson.c -o validatejson.s32x`
  and run:
  `./tools/emulator/slow32-fast --mmio 0x10000 validatejson.s32x file.json`.
