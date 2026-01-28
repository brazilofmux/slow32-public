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
