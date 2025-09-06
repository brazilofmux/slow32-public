# SLOW-32 Toolchain Overview

The SLOW-32 toolchain provides everything needed to write, compile, and run programs for the SLOW-32 architecture.

## Components

### Core Tools

#### Assembler (`slow32asm`)
Converts SLOW-32 assembly language into object files.

- **Input**: `.s` assembly files
- **Output**: `.s32o` object files
- **Features**:
  - Symbol resolution (labels and references)
  - Relocation generation (R_SLOW32_ABS32, R_SLOW32_HI20, R_SLOW32_LO12)
  - Section management (.text, .data, .rodata, .bss)
  - Basic expression evaluation (%hi/%lo for addresses)
  - Two-pass assembly for forward references

```bash
slow32asm -o output.s32o input.s
# or legacy: slow32asm input.s output.s32o
```

#### Linker (`s32-ld`)
Links object files into executables.

- **Input**: `.s32o` object files
- **Output**: `.s32x` executables
- **Features**:
  - Symbol resolution across files
  - Relocation processing
  - Section merging
  - Dead code elimination

```bash
s32-ld -o program.s32x crt0.s32o main.s32o lib.s32o
```

#### Emulators

**Reference Emulator (`slow32`)**
- Full debugging support
- Instruction tracing
- Memory watching
- Cycle-accurate simulation

**Fast Emulator (`slow32-fast`)**
- Optimized for speed (~350M instructions/sec)
- Minimal debugging
- Production runs

```bash
# Debug mode
slow32 -t program.s32x  # trace
slow32 -s program.s32x  # step
slow32 -r program.s32x  # register changes

# Fast mode
slow32-fast program.s32x
```

### LLVM Backend

#### Clang Frontend
Native SLOW-32 target support:
```bash
clang --target slow32-unknown-none -S -emit-llvm program.c
```

#### LLVM Code Generator (`llc`)
Lowers LLVM IR to SLOW-32 assembly:
```bash
llc -mtriple=slow32-unknown-none program.ll -o program.s
```

### Analysis Tools

#### Disassembler (`slow32dis`)
```bash
slow32dis program.s32x [start_addr] [end_addr]
```

#### Object Dumper (`s32-objdump`)
```bash
s32-objdump file.s32o
```

#### Executable Dumper (`s32-exedump`)
```bash
s32-exedump file.s32x
```

## File Formats

### Source Files

**.s** - Assembly source
```asm
        .section .text
        .globl _start
_start:
        li r1, 42
        halt
```

**.c** - C source (via LLVM)
```c
int main() {
    return 42;
}
```

### Binary Formats

**.s32o** - Object file
- Magic: `S32O`
- Contains: Code, data, symbols, relocations
- Linkable, not executable

**.s32x** - Executable
- Magic: `S32X`
- Contains: Merged sections, resolved symbols
- Ready to run

## Runtime Libraries

### crt0.s32o
C runtime startup:
- Sets up stack
- Calls main()
- Handles return value
- **Must be first in link order**

### intrinsics.s32o
Built-in functions:
- `memcpy` - Memory copy
- `memset` - Memory fill
- `__muldi3` - 64-bit multiply (planned)
- `__divdi3` - 64-bit divide (planned)

### debug_char.s32o
Debug output support:
- Character output via DEBUG instruction
- Basic string printing

## Compilation Pipeline

```
C Source (.c)
    ↓ [clang --target slow32-unknown-none]
LLVM IR (.ll)
    ↓ [llc -mtriple=slow32-unknown-none]
Assembly (.s)
    ↓ [slow32asm]
Object File (.s32o)
    ↓ [s32-ld with crt0.s32o, intrinsics.s32o]
Executable (.s32x)
    ↓ [slow32 or slow32-fast]
Execution
```

## Build System Integration

### Makefile Example
```makefile
CC = ~/llvm-project/build/bin/clang
LLC = ~/llvm-project/build/bin/llc
AS = ~/slow-32/assembler/slow32asm
LD = ~/slow-32/linker/s32-ld
EMU = ~/slow-32/emulator/slow32
RUNTIME = ~/slow-32/runtime

TARGET = slow32-unknown-none
CFLAGS = --target=$(TARGET) -S -emit-llvm -O1
LLCFLAGS = -mtriple=$(TARGET)

%.ll: %.c
	$(CC) $(CFLAGS) $< -o $@

%.s: %.ll
	$(LLC) $(LLCFLAGS) $< -o $@

%.s32o: %.s
	$(AS) -o $@ $<

%.s32x: %.s32o
	$(LD) -o $@ $(RUNTIME)/crt0.s32o $< $(RUNTIME)/intrinsics.s32o

run-%: %.s32x
	$(EMU) $<

clean:
	rm -f *.ll *.s *.s32o *.s32x
```

### CMake Example
```cmake
# FindSLOW32.cmake
find_program(SLOW32_CLANG clang
    PATHS ~/llvm-project/build/bin
    REQUIRED)

find_program(SLOW32_LLC llc
    PATHS ~/llvm-project/build/bin
    REQUIRED)

find_program(SLOW32_ASM slow32asm
    PATHS ~/slow-32/assembler
    REQUIRED)

find_program(SLOW32_LD s32-ld
    PATHS ~/slow-32/linker
    REQUIRED)

set(SLOW32_RUNTIME ~/slow-32/runtime)
set(SLOW32_TARGET slow32-unknown-none)
```

## Environment Setup

### Recommended Directory Structure
```
~/
├── slow-32/           # Core toolchain
│   ├── assembler/
│   ├── linker/
│   ├── emulator/
│   └── runtime/
├── llvm-project/      # LLVM with SLOW-32 backend
│   └── build/
└── slow32-projects/   # Your projects
    └── my-app/
```

### Environment Variables (Optional)
```bash
# Add to ~/.bashrc
export SLOW32_HOME=~/slow-32
export SLOW32_LLVM=~/llvm-project/build
export PATH=$SLOW32_HOME/assembler:$SLOW32_HOME/linker:$PATH
export PATH=$SLOW32_HOME/emulator:$SLOW32_LLVM/bin:$PATH
```

## Optimization Tips

1. **Always use -O1 or -O2** with Clang
2. **Link order**: crt0.s32o must be first
3. **Debug builds**: Use `-g` flag (planned)
4. **Profile first**: Use trace mode to find hotspots
5. **Memory alignment**: Word-align data for performance

## Limitations

- No floating point (use soft-float)
- No SIMD instructions
- Limited 64-bit support
- Single-threaded only
- Debug output only (no stdin yet)

## See Also

- [Getting Started](01-getting-started.md)
- [Programmer's Guide](20-programmers-guide.md)
- [LLVM Backend](35-llvm-backend.md)