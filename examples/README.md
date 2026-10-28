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
  `./tools/emulator/slow32-fast validatejson.s32x file.json` (the MMIO size
  comes from the executable header).  `make` here builds it, the Ragel CSV
  validator `validatecsv_ragel.c` and its scheduler/serial variants;
  `make parity` checks the two CSV variants agree on `csvtests/`.

- `vecscope.c`, `fire.c`, `sprites.c` — the tube demos (vector, framebuffer,
  PPU); `./build-tube-demos.sh` builds all three.  Watch them with
  `tools/s32-crt-mac` on macOS; `tools/s32-crt` draws vec frames in a
  terminal.

- `cpp/` — small C++ programs (classes, templates, lambdas, exceptions);
  `cpp/test_all.sh` builds and runs them.

- `csvbench/` — splits CSV validation across several emulator instances
  (`gen.sh`, then `bench.sh`).
