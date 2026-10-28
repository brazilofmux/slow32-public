
# Repository Guidelines

## Toolchain & Environment
This project assumes an LLVM built with the SLOW-32 backend in `~/llvm-project/build/bin` (the backend's sources are mirrored in `llvm-backend/`). Run `make` once to produce the assembler, linker, emulators and DBT inside `tools/`. Export `LLVM_BIN` or update your `PATH` so helper scripts (for example `regression/run-tests.sh`) can locate those binaries. With no local LLVM, the `slow32:toolchain` container image (`docker.io/hurriedreformist/slow32:toolchain`) carries one.

## Project Structure & Module Organization
Core toolchain sources live in `tools/` (assembler, linker, emulators, DBT, utilities). The LLVM backend is mirrored in `llvm-backend/` (and the clang driver in `clang-target/`); QEMU and Free Pascal backends sit in `qemu-backend/` and `fpc-backend/`. Shared formats and headers sit in `common/`, while `runtime/` hosts crt0 and the libc/intrinsics archives. `selfhost/` is the self-hosting bootstrap chain ending in stage08 cc; `cobol/`, `fortran/`, `forth/` and `islisp/` are language fronts; most other top-level directories are applications. Regression inputs belong to `regression/tests/`, with artifacts in `regression/results/`. `docs/SPEC.md` is the normative machine specification; other reference material is in `docs/`.

## Build, Test, and Development Commands
`make` compiles the full toolchain; target specific layers with `make runtime`, `make assembler`, `make emulator` or `make dbt`. The pattern target `make run-<name>` compiles `<name>.c` through to an executable and runs it via the emulator; choose the I/O backend with `LIBC=mmio`. `scripts/compile.sh prog.c [out.s32x]` runs the clang → llc → assembler → linker pipeline, and the clang driver does it in one step: `SLOW32_HOME=$PWD clang --target=slow32-unknown-none prog.c -o prog.s32x`.

## Coding Style & Naming Conventions
Follow the existing C style: four-space indentation, brace-open on the same line, `snake_case` for functions and locals, and `ALL_CAPS` for constants. Prefer `stdint.h` types and `//` comments for inline notes. Shell scripts are Bash with `set -e` and descriptive helper functions—mirror that structure when automating. No formatter is checked in, so rely on compiler warnings (`-Wall -Wextra`) to catch drift.

## Testing Guidelines
`make test` provides a smoke test for the assembler and emulator. Run `./regression/run-tests.sh` for broader coverage and `./regression/run-differential.sh` to run every test on every engine. A test is a directory `regression/tests/<category>-<name>/` holding `test.c` (or `test.s`, `test.cpp`) and `expected.txt`; optional files such as `clang-opt` (e.g. `-O2`; the default is `-O0`) and `use_mmio` change how it is built. Changes to engines, the LLVM backend, the C libraries or stage08 have further harnesses (differentials, fuzzers, the LLVM machine verifier); `CLAUDE.md`, "Testing Commands", lists each with when to run it.

## Commit & Pull Request Guidelines
Write commit subjects as a component prefix and a plain statement of what changed (`LLVM backend: the constant 0 is r0`, `libcob: the 32K stream buffer for output only`), keep the first line under 72 characters, and add wrapped detail on the why and how it was verified. Cite in-tree issue items as `DBT-14` or `selfhost ISSUES-48`, never a bare `#N` (that is reserved for GitHub issues). Pull requests should cover the problem statement, impacted modules, verification evidence (`make`, regression and differential results), and any behavior changes such as emulator output.
