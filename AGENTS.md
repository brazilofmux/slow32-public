# Repository Guidelines

## Toolchain & Environment
This project assumes LLVM tools in `~/llvm-project/build/bin`. Run `make` once to produce the custom assembler, linker, and emulator inside `tools/`. Export `LLVM_BIN` or update your `PATH` so helper scripts (for example `regression/run-tests.sh`) can locate those binaries.

## Project Structure & Module Organization
Core toolchain sources live in `tools/` (assembler, emulator, linker, utilities). The LLVM backend and standalone compiler reside in `llvm-backend/` (see `standalone/` for the driver). Shared formats and headers sit in `common/`, while `runtime/` hosts startup objects and the libc/debug/MMIO archives. Regression inputs belong to `regression/tests/`, with artifacts in `regression/results/`. Additional reference material is in `docs/`, and `examples/` plus the top-level `test_*.c` files provide sample workloads.

## Build, Test, and Development Commands
`make` compiles the full toolchain; target specific layers with `make runtime`, `make assembler`, or `make emulator`. The pattern target `make run-test` compiles `test.c` through to an executable and runs it via the emulator. Toggle I/O backends during linking with `LIBC=mmio`. Once `llvm-backend/standalone/slow32-compile` exists, the `slow32cc` driver wraps the pipeline (`./slow32cc hello.c -O2 -o hello.s32x`).

## Coding Style & Naming Conventions
Follow the existing C style: four-space indentation, brace-open on the same line, `snake_case` for functions and locals, and `ALL_CAPS` for constants. Prefer `stdint.h` types and `//` comments for inline notes. Assembly files keep uppercase mnemonics with tab-indented operands. Shell scripts are Bash with `set -e` and descriptive helper functions—mirror that structure when automating. No formatter is checked in, so rely on compiler warnings (`-Wall -Wextra`) to catch drift.

## Testing Guidelines
`make test` provides a smoke test for the assembler and emulator. Run `./regression/run-tests.sh` for broader coverage; it expects LLVM tools in `~/llvm-project/build/bin` and emits per-case logs into `regression/results/<case>/`. New scenarios belong under the closest category directory with a `test.c` and optional `expected.txt`. When touching runtime or libc code, verify both DEBUG and MMIO variants by rebuilding with `LIBC=mmio`.

## Commit & Pull Request Guidelines
Recent history favors concise, descriptive summaries (`Fix printf decimal formatting...`, `BREAKTHROUGH: ...`). Write new commits in the imperative mood, keep the first line under 72 characters, and add wrapped detail only when context is non-obvious. Pull requests should cover the problem statement, impacted modules, verification evidence (`make`, `make test`, regression logs), and any behavior changes such as emulator output. Note toolchain version requirements or environment tweaks so reviewers can reproduce results quickly.
