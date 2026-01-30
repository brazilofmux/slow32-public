# SLOW-32 Context for Gemini

## Project Overview
**SLOW-32** is a deliberately inefficient 32-bit RISC CPU architecture designed for educational purposes and sandboxed execution. It features a complete, custom-built toolchain including an LLVM backend, assembler, linker, and emulator.

- **Primary Goal:** Educational CPU design, verified with a complete C-to-Silicon-like stack.
- **Key Characteristics:** 
    - 32-bit fixed-width instructions.
    - No condition codes (comparisons return 0/1).
    - W^X memory protection (Execute-only code).
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
- **Memory Layout:**
    - Code: `0x00000000` (Execute-only)
    - Data: `0x00100000` (Read/Write)
    - Stack: `0x0FFFFFF0` (Grows down)
    - MMIO: `0x10000000+`
- **ISA:** RISC-V like but simplified. See `docs/INSTRUCTION-SET.md`.

## Toolchain & Directory Structure
The project relies on an external LLVM build (assumed at `~/llvm-project/build/bin`) for the frontend, while the rest of the stack is custom.

- **`llvm-backend/`**: LLVM target definitions and patches.
- **`tools/`**:
    - `assembler/`: Custom two-pass assembler (`slow32asm`).
    - `linker/`: Custom linker (`s32-ld`).
    - `emulator/`: CPU emulators (`slow32` and `slow32-fast`).
    - `utilities/`: Binary analysis tools (`slow32dump`, `slow32dis`).
- **`runtime/`**: C runtime (`crt0`), standard library (`libc`), and intrinsics (`libs32`).
- **`regression/`**: Comprehensive regression test suite.

## Workflow & Commands

### Building the Toolchain
```bash
make          # Builds assembler, linker, emulator, and runtime
make clean    # Cleans build artifacts
```

### Compiling & Running C Programs
The standard flow is C -> LLVM IR -> ASM -> Object -> Executable.

**Using Helper Script (Recommended):**
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
*   **Quick Test:** `make test`
*   **Regression Suite (Critical):** `regression/run-tests.sh` (Must pass before committing backend changes).
*   **Docker:** `scripts/run-in-docker.sh` is available for isolated execution.

## Development Conventions
*   **Code Style:** Standard C style (4-space indent, snake_case).
*   **Commits:** Imperative mood, clear descriptions.
*   **Testing:** Always run regression tests after touching `llvm-backend` or `runtime`.
*   **I/O:** Default is `DEBUG` instruction (simple). Use `LIBC=mmio` for MMIO-based high-performance I/O.

## Key Files
*   `README.md`: General project info.
*   `CLAUDE.md`: Quick reference for commands and status.
*   `docs/IMPROVEMENTS.md`: Known issues and roadmap.
*   `Makefile`: Main build script.

## Gemini Added Memories
- The SLOW-32 assembler syntax for the `stw` instruction is `stw base, src, offset`, which differs from the standard RISC-V `sw src, offset(base)`.
- The slow32asm assembler seems to crash (segfault) when encountering .byte sequences for strings that contain certain characters or in specific contexts, or perhaps it's a general issue with my extensive editing of the assembly file. It crashed after I replaced .ascii strings with .byte sequences.
- The `rs` directory contains a native C++ Reed-Solomon toolchain that is for reference only. It is ignored by `.gitignore` and is not a primary part of the project's build process.
- The command to refresh the TinyMUX NuGet token is: aws codeartifact get-authorization-token --domain tinymux --domain-owner 274462252673 --query authorizationToken --output text
- When linking with `libc_mmio.s32a`, the linker (`s32-ld`) requires the `--mmio <SIZE>` flag (e.g., `--mmio 0x10000`) to define the MMIO memory space, otherwise execution may fail with a memory fault.
- Fixed stack overflow risks in Regal by moving large `EditJournal` structs to static memory and adding recursion depth limits to JSON and expression parsers.
- The `malloc` implementation in `runtime/malloc.c` has been upgraded to use boundary tags and a doubly-linked free list, enabling O(1) free and coalescing. A regression test `regression/tests/stdlib-malloc` has been added.
- The `malloc` implementation has been further upgraded to use Segregated Free Lists (8 bins) with Sentinel Nodes, significantly optimizing small allocations. This builds upon the previous boundary tag implementation.
- `__heap_start` is 4KB aligned by the linker (`s32-ld`), satisfying the 8-byte alignment requirement for `malloc`.
- The `memset` implementation in `runtime/intrinsics.s` has been optimized to use `beq` instead of `bne` for loops, adhering to SLOW-32 safety guidelines.
- The `slow32-dbt` emulator is currently the fastest implementation, achieving 1.5 BIPS (Billion Instructions Per Second).
- **Runtime I/O Overhaul (2026-01-29):**
    - **Buffered I/O:** `stdio.c` now implements full buffering (`_IOFBF`) for files and line buffering (`_IOLBF`) for `stdout`. `stderr` remains unbuffered.
    - **Performance:** `fwrite` chunks large writes into `S32_MMIO_DATA_CAPACITY` blocks and uses `S32_MMIO_OP_WRITE` for all streams, significantly improving `printf` and bulk write performance.
    - **Safety:** `putint` stack corruption fixed (buffer moved to valid stack frame).
    - **Completeness:** Added `stdlib` (abs, atoi, rand) and `time` (gmtime, mktime) functions in `runtime/stdlib_missing.c` and `runtime/time_extra.c`.