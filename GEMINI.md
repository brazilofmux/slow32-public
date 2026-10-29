# SLOW-32 Context for Gemini

## Project Overview
**SLOW-32** is a deliberately inefficient 32-bit RISC CPU architecture designed for educational purposes and sandboxed execution. It features a complete, custom-built toolchain including an LLVM backend, assembler, linker, and emulator.

- **Primary Goal:** Educational CPU design, verified with a complete C-to-Silicon-like stack.
- **Key Characteristics:** 
    - 32-bit fixed-width instructions.
    - No condition codes (comparisons return 0/1).
    - W^X memory protection (code is not writable; instructions are fetched only from code, which loads may read).
    - Single-ported memory (deliberately slow).
    - Two I/O modes: `DEBUG` instruction (char output) and MMIO ring buffer.

## Architecture Details

- **Registers:** 32 registers (`r0` hardwired to 0).
    - `r0`: Zero
    - `r1-r2`: Return values
    - `r3-r10`: Arguments
    - `r29`: Stack Pointer (SP)
    - `r30`: Frame Pointer (FP)
    - `r31`: Link Register (LR)
- **Memory Layout:** (normative: `docs/SPEC.md` section 6)
    - Code from `0x00000000` (fetch only from here; not writable)
    - Read-only data, then data/BSS/heap, packed after it on 4 KB boundaries
    - MMIO: a 64 KB window after the heap when linked with `--mmio`; found through `__mmio_base`, not a fixed address
    - Stack: `0x0FFFFFF0` (Grows down)
- **ISA:** RISC-V like but simplified. See `docs/SPEC.md` (normative) and `docs/INSTRUCTION-SET.md`.

## Toolchain & Directory Structure
The project relies on an external LLVM build (assumed at `~/llvm-project/build/bin`) for the frontend, while the rest of the stack is custom.

- **`llvm-backend/`**: LLVM target definitions and patches (mirrored from `~/llvm-project`).
- **`tools/`**:
    - `assembler/`: Custom two-pass assembler (`slow32asm`).
    - `linker/`: Custom linker (`s32-ld`).
    - `emulator/`: Reference interpreter (`slow32`) and `slow32-fast`.
    - `dbt/`: Dynamic binary translator (`slow32-dbt`), x86-64 and AArch64.
    - `utilities/`: Binary analysis tools (`slow32dump`, `slow32dis`, `s32-ar`).
- **`runtime/`**: C runtime (`crt0`), standard library (`libc`), and intrinsics (`libs32`).
- **`selfhost/`**: Self-hosting bootstrap chain ending in stage08 cc.
- **`cobol/`, `fortran/`, `forth/`, `islisp/`**: Language fronts.
- **`regression/`**: Regression suite, cross-engine differential harnesses, fuzzers.

## Workflow & Commands

### Building the Toolchain
```bash
make          # Builds assembler, linker, emulators, DBT, and runtime
make clean    # Cleans build artifacts
```

### Compiling & Running C Programs
The standard flow is C -> LLVM IR -> ASM -> Object -> Executable.

**Using the clang driver (one step):**
```bash
SLOW32_HOME=$PWD ~/llvm-project/build/bin/clang --target=slow32-unknown-none -O2 program.c -o program.s32x
./tools/emulator/slow32 program.s32x
```

**Using Helper Script:**
```bash
./scripts/compile.sh program.c [output.s32x] [--mmio SIZE]
./tools/emulator/slow32 program.s32x
```

**Manual Steps (Native LLVM Target):**
```bash
# 1. Compile to LLVM IR
~/llvm-project/build/bin/clang -target slow32-unknown-none -S -emit-llvm -O2 -Iruntime/include program.c -o program.ll

# 2. Compile to Assembly
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none program.ll -o program.s

# 3. Assemble
./tools/assembler/slow32asm program.s program.s32o

# 4. Link (crt0 must be first)
./tools/linker/s32-ld -o program.s32x runtime/crt0.s32o program.s32o runtime/libc_debug.s32a runtime/libs32.s32a

# 5. Run
./tools/emulator/slow32 program.s32x
```

### Testing

-   **Quick Test:** `make test`
-   **Regression Suite (Critical):** `regression/run-tests.sh` (Must pass before committing backend changes).
-   **Every engine:** `regression/run-differential.sh`; the other harnesses are listed in `CLAUDE.md`, "Testing Commands".
-   **Docker:** `scripts/run-in-docker.sh` is available for isolated execution.

## Development Conventions

-   **Code Style:** Standard C style (4-space indent, snake_case).
-   **Commits:** a component prefix and a plain statement of what changed (`LLVM backend: the constant 0 is r0`). Cite in-tree issue items as `DBT-14` or `selfhost ISSUES-48`, never a bare `#N` (that is reserved for GitHub issues, and `fixes #N` closes one).
-   **Private data:** Nothing from the user's private ledger (`~/majesty`: ledger codes and the report names built on them, account numbers, amounts, payees) and no private repository names go into files or commit messages: this tree is copied to a public mirror. See `CLAUDE.md`, "Public mirror".
-   **Testing:** Always run regression tests after touching `llvm-backend` or `runtime`.
-   **I/O:** Default is `DEBUG` instruction (simple). Use `LIBC=mmio` for MMIO-based high-performance I/O.

## Key Files

-   `README.md`: General project info.
-   `CLAUDE.md`: Quick reference for commands and status.
-   `docs/IMPROVEMENTS.md`: Known issues and roadmap.
-   `Makefile`: Main build script.

## Gemini Added Memories

- The SLOW-32 assembler syntax for stores is `stw base+offset, src` (e.g. `stw sp+4, r3`) and for loads `ldw rd, base+offset`, which differs from the standard RISC-V `sw src, offset(base)`.
- When linking with `libc_mmio.s32a`, the linker (`s32-ld`) requires the `--mmio <SIZE>` flag (e.g., `--mmio 0x10000`) to define the MMIO memory space, otherwise execution may fail with a memory fault.
- The `malloc` implementation in `runtime/malloc.c` has been upgraded to use boundary tags and a doubly-linked free list, enabling O(1) free and coalescing. A regression test `regression/tests/stdlib-malloc` has been added.
- The `malloc` implementation has been further upgraded to use Segregated Free Lists (8 bins) with Sentinel Nodes, significantly optimizing small allocations. This builds upon the previous boundary tag implementation.
- `__heap_start` is 4KB aligned by the linker (`s32-ld`), satisfying the 8-byte alignment requirement for `malloc`.
- The `memset` implementation in `runtime/intrinsics.s` has been optimized to use `beq` instead of `bne` for loops, adhering to SLOW-32 safety guidelines.
- The `slow32-dbt` emulator is the fastest implementation, up to ~8.3 BIPS on an Apple M5 Max (2026-08-23); see `docs/EMULATORS.md`.
- **Runtime I/O Overhaul (2026-01-29):**
    - **Buffered I/O:** `stdio.c` now implements full buffering (`_IOFBF`) for files and line buffering (`_IOLBF`) for `stdout`. `stderr` remains unbuffered.
    - **Performance:** `fwrite` chunks large writes into `S32_MMIO_DATA_CAPACITY` blocks and uses `S32_MMIO_OP_WRITE` for all streams, significantly improving `printf` and bulk write performance.
    - **Safety:** `putint` stack corruption fixed (buffer moved to valid stack frame).
    - **Completeness:** Added `stdlib` (abs, atoi, rand) and `time` (gmtime, mktime) functions (now in `runtime/stdlib_utils.c` and `runtime/time_extra.c`).
