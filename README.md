# SLOW-32 Project

A deliberately inefficient 32-bit RISC CPU architecture with complete toolchain.

**Built in under 2 hours** - From spec to working C compiler!
**Purpose**: Educational CPU design and sandboxed compute engine
**Status**: Active maintenance-ready baseline with all optimization levels (-O0, -O1, -O2) working

## Quick Start

```bash
# Build everything
make

# Compile C to executable (Native LLVM target)
# Note: All optimization levels (-O0, -O1, -O2) are supported
~/llvm-project/build/bin/clang -target slow32-unknown-none -S -emit-llvm -O2 -Iruntime/include program.c -o program.ll
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none program.ll -o program.s
./tools/assembler/slow32asm program.s program.s32o
./tools/linker/s32-ld -o program.s32x runtime/crt0.s32o program.s32o runtime/libc_debug.s32a runtime/libs32.s32a
./tools/emulator/slow32 program.s32x

# Or assemble and run assembly directly
./tools/assembler/slow32asm program.s program.s32o
./tools/linker/s32-ld -o program.s32x runtime/crt0.s32o program.s32o runtime/libc_debug.s32a runtime/libs32.s32a
./tools/emulator/slow32 program.s32x
```

## Architecture Overview

- **32-bit RISC-like ISA** with 32 registers (r0 hardwired to zero)
- **No condition codes** - comparisons return 0/1 in GPR
- **Fixed 32-bit instructions**
- **W^X memory protection** - code segment is execute-only
- **Single-ported memory** (deliberately slow!)
- **DEBUG instruction** for character output; **MMIO ring buffers** for full I/O
- **Sparse memory allocation** - Only allocates touched pages (99.4% memory savings!)
- **Performance**: ~45 MIPS (slow32), ~220 MIPS (slow32-fast), ~3.5 BIPS (slow32-dbt, only 2.8x native)

### Register Convention
- `r0`: Always zero
- `r1-r2`: Return values
- `r3-r10`: Function arguments
- `r11-r28`: General purpose
- `r29`: Stack pointer (sp)
- `r30`: Frame pointer (fp)
- `r31`: Link register (lr)

## Instruction Set

### Arithmetic (R-type)
- `add`, `sub`, `and`, `or`, `xor` - Basic arithmetic/logic
- `sll`, `srl`, `sra` - Shifts
- `mul`, `mulh`, `div`, `rem` - Multiplication/division

### Comparison (R-type)
- `slt`, `sltu` - Set less than (signed/unsigned)
- `seq` - Set equal (rd = rs1 == rs2 ? 1 : 0)
- `sne` - Set not equal (rd = rs1 != rs2 ? 1 : 0)
- `sgt`, `sgtu` - Set greater than (signed/unsigned)
- `sle`, `sleu` - Set less or equal (signed/unsigned)
- `sge`, `sgeu` - Set greater or equal (signed/unsigned)

### Immediate (I-type)
- `addi`, `ori`, `andi` - Immediate operations
- `slli`, `srli`, `srai` - Immediate shifts
- `slti`, `sltiu` - Set less than immediate
- `lui` - Load upper immediate

### Memory Operations
- `ldb`, `ldh`, `ldw` - Load byte/half/word (signed)
- `ldbu`, `ldhu` - Load byte/half (unsigned)
- `stb`, `sth`, `stw` - Store byte/half/word

### Control Flow
- `jal` - Jump and link
- `jalr` - Jump and link register
- `beq`, `bne` - Branch equal/not equal
- `blt`, `bge` - Branch less than/greater or equal
- `bltu`, `bgeu` - Branch unsigned comparisons

### Floating-Point (R-type, values in GPRs)
- `fadd.s`, `fsub.s`, `fmul.s`, `fdiv.s`, `fsqrt.s` - f32 arithmetic
- `feq.s`, `flt.s`, `fle.s` - f32 comparisons
- `fcvt.w.s`, `fcvt.wu.s`, `fcvt.s.w`, `fcvt.s.wu` - f32 ↔ int conversions
- `fadd.d`, `fsub.d`, `fmul.d`, `fdiv.d`, `fsqrt.d` - f64 arithmetic (register pairs)
- `feq.d`, `flt.d`, `fle.d` - f64 comparisons
- `fcvt.w.d`, `fcvt.wu.d`, `fcvt.d.w`, `fcvt.d.wu` - f64 ↔ int conversions
- `fcvt.s.d`, `fcvt.d.s` - f32 ↔ f64 conversions

### Special Instructions
- `nop` - No operation
- `yield` - Waste cycles
- `debug` - Output character in rs1
- `halt` - Stop execution

## Assembler Directives

Following RISC-V/GNU conventions:

- `.text` / `.code` - Code section
- `.data` - Data section
- `.byte` - 8-bit values
- `.half` - 16-bit values
- `.word` - 32-bit values
- `.string` - Null-terminated string
- `.ascii` - String without null terminator
- `.global` - Global symbol

## Memory Layout

### Automatic Compact Mode
The linker automatically detects small programs and creates ultra-compact layouts:
- **Tiny programs**: As small as 9KB total (4KB code + 4KB heap + 1KB stack)
- **Page-aligned**: 4KB boundaries for hardware memory protection
- **Configurable**: Full control with `--code-size`, `--stack-size`, etc.

### Default Layout
| Address Range | Size | Description |
|--------------|------|-------------|
| 0x00000000 - 0x000FFFFF | 1MB | Code segment (execute-only) |
| 0x00100000 - 0x0FFFFFFF | 255MB | Data segment (read/write) |
| 0x10000000+ | - | MMIO region (future) |
| Stack: 0x0FFFFFF0 | - | Grows downward |

## Performance Characteristics

- Memory operations: 3 cycles
- Multiplication: 32 cycles
- Division/Remainder: 64 cycles
- All other operations: 1 cycle
- YIELD instruction: Variable cycle waste

## Project Structure

```
slow-32/
├── tools/
│   ├── emulator/     # CPU emulators (slow32, slow32-fast) with W^X protection
│   ├── assembler/    # Two-pass assembler with relocation support
│   ├── linker/       # Linker with symbol resolution
│   ├── utilities/    # Binary analysis tools (slow32dump, slow32dis, s32-ar)
│   └── dbt/          # Dynamic binary translator (x86-64 JIT)
├── runtime/          # C runtime (crt0), intrinsics, and standard library
│   └── include/      # C standard library headers
├── llvm-backend/     # LLVM backend for native clang/llc support
├── common/           # Shared format definitions (.s32o, .s32x)
├── regression/       # Regression test suite
├── scripts/          # Helper scripts (compile.sh, test-quick.sh)
└── docs/             # Documentation
    ├── INSTRUCTION-SET.md    # Complete ISA reference
    ├── file-formats.md       # Object and executable formats
    └── IMPROVEMENTS.md       # Known issues and improvements
```

## Example Programs

### Hello World (Assembly)
```asm
.data
    msg: .string "Hello, World!\n"

.text
.global _start
_start:
    lui r10, 0x100      # Load data segment base
    add r11, r10, r0    # Initialize pointer
    
print_loop:
    ldbu r12, r11+0     # Load character
    seq r13, r12, r0    # Check for null
    bne r13, r0, done   # Exit if null
    debug r12           # Print character
    addi r11, r11, 1    # Next character
    beq r0, r0, print_loop
    
done:
    halt
```

### C Example
```c
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main() {
    return factorial(5);  // Returns 120
}
```

## Current Status

- ✅ **Complete toolchain** - C → LLVM IR → Assembly → Object → Linked Executable
- ✅ **Native Clang target** - `-target slow32-unknown-none` (single dash)
- ✅ **All optimization levels** - -O0, -O1, -O2 fully working
- ✅ **CPU emulator** - ~350M inst/sec (interpreter), ~3.5B inst/sec (DBT) with W^X protection
- ✅ **Assembler** - Two-pass with labels, relocations, standard directives
- ✅ **Linker** - Symbol resolution, HI20/LO12 relocations, proper archives
- ✅ **LLVM backend** - PHI nodes, intrinsics, varargs, jump tables, 64-bit integers
- ✅ **Runtime** - crt0, printf with varargs, memcpy/memset, 64-bit builtins
- ✅ **IEEE 754 floating-point** - f32 and f64 via hardware FP instructions + software dtoa/printf(%f/%e/%g)
- ✅ **Regression tests** - All 45/45 passing
- ✅ **Tools** - objdump for object files, exedump for executables  

## Known Limitations

- No scanf/sscanf (declared but not implemented)
- See `docs/IMPROVEMENTS.md` for detailed issues and fixes

## Building from Source

```bash
# Prerequisites: make, gcc (plus LLVM with SLOW-32 backend for compiling C)

# Build everything
make

# Or build components individually
make emulator     # Builds slow32 and slow32-fast
make assembler    # Builds slow32asm
make linker       # Builds s32-ld
make utilities    # Builds slow32dump, slow32dis, s32-ar
make dbt          # Builds slow32-dbt (dynamic binary translator)
make runtime      # Builds crt0.s32o, libs32.s32a, libc_debug.s32a, libc_mmio.s32a
```

## Documentation

- [Instruction Set Reference](docs/INSTRUCTION-SET.md) - Complete ISA documentation
- [File Formats](docs/file-formats.md) - Object (.s32o) and executable (.s32x) formats
- [Improvements](docs/IMPROVEMENTS.md) - Known issues and suggested fixes
- [CLAUDE.md](CLAUDE.md) - AI assistant instructions and quick reference

## License

Educational project - free to use for learning purposes.
