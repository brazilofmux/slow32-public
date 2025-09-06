# Getting Started with SLOW-32

This guide will walk you through setting up and using the SLOW-32 toolchain.

## Prerequisites

- Linux system (tested on Ubuntu/Debian)
- LLVM 19+ with Clang
- CMake 3.20+
- Make or Ninja
- Basic development tools (gcc, g++, git)

## Repository Structure

The SLOW-32 project consists of three main repositories:

1. **slow-32** - Core toolchain (assembler, linker, emulator, runtime)
2. **llvm-project** - LLVM backend for SLOW-32
3. **slow32-public** - Documentation and examples (this repository)

## Quick Start (Using Pre-built Tools)

If you have the SLOW-32 toolchain already built:

```bash
# 1. Write a simple assembly program
cat > hello.s << 'EOF'
        .section .rodata
msg:    .ascii  "Hello, SLOW-32!\n\0"

        .section .text
        .globl  _start
_start:
        li      r3, msg
.loop:
        ldbu    r2, r3+0
        beq     r2, r0, .done
        debug   r2
        addi    r3, r3, 1
        jal     r0, .loop
.done:
        halt
EOF

# 2. Assemble
~/slow-32/assembler/slow32asm -o hello.s32o hello.s

# 3. Link
~/slow-32/linker/s32-ld -o hello.s32x ~/slow-32/runtime/crt0.s32o hello.s32o

# 4. Run
~/slow-32/emulator/slow32 hello.s32x
```

## Building from Source

### Step 1: Build the Core Toolchain

```bash
# Clone and build slow-32 toolchain
git clone [repository-url] ~/slow-32
cd ~/slow-32
make

# This builds:
# - assembler/slow32asm - The assembler
# - linker/s32-ld - The linker
# - emulator/slow32 - Reference emulator
# - emulator/slow32-fast - Optimized emulator
# - runtime/*.s32o - Runtime libraries
```

### Step 2: Build the LLVM Backend

```bash
# Clone LLVM with SLOW-32 backend
git clone [repository-url] ~/llvm-project
cd ~/llvm-project
mkdir build && cd build

# Configure LLVM with SLOW-32 as experimental target
cmake -S ../llvm -B . -G "Unix Makefiles" \
  -DCMAKE_BUILD_TYPE=Release \
  -DLLVM_TARGETS_TO_BUILD="X86;RISCV" \
  -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD="SLOW32" \
  -DLLVM_ENABLE_PROJECTS="clang"

# Build (this takes a while)
make -j$(nproc) llc clang

# Verify the backend
./bin/llc -version | grep -i slow32
```

## Compiling C Programs

### Method 1: Using Clang with SLOW-32 Target (Recommended)

```bash
# Compile C to SLOW-32 assembly
~/llvm-project/build/bin/clang --target slow32-unknown-none \
    -S -emit-llvm -O1 program.c -o program.ll

# Lower to assembly
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none \
    program.ll -o program.s

# Assemble
~/slow-32/assembler/slow32asm -o program.s32o program.s

# Link with runtime
~/slow-32/linker/s32-ld -o program.s32x \
    ~/slow-32/runtime/crt0.s32o \
    program.s32o \
    ~/slow-32/runtime/intrinsics.s32o

# Run
~/slow-32/emulator/slow32 program.s32x
```

### Method 2: Using the Compile Script (Convenience)

Create a compile script:

```bash
#!/bin/bash
# save as: slow32-compile.sh

if [ $# -ne 1 ]; then
    echo "Usage: $0 <program.c>"
    exit 1
fi

BASENAME=$(basename "$1" .c)
LLVM_BIN=~/llvm-project/build/bin
SLOW32=~/slow-32

# Compile
$LLVM_BIN/clang --target slow32-unknown-none -S -emit-llvm -O1 "$1" -o "$BASENAME.ll"
$LLVM_BIN/llc -mtriple=slow32-unknown-none "$BASENAME.ll" -o "$BASENAME.s"

# Assemble and link
$SLOW32/assembler/slow32asm -o "$BASENAME.s32o" "$BASENAME.s"
$SLOW32/linker/s32-ld -o "$BASENAME.s32x" \
    $SLOW32/runtime/crt0.s32o \
    "$BASENAME.s32o" \
    $SLOW32/runtime/intrinsics.s32o

echo "Created $BASENAME.s32x"
```

## Debugging Programs

The SLOW-32 emulator has comprehensive debugging features:

```bash
# Trace every instruction
~/slow-32/emulator/slow32 -t program.s32x

# Show register changes only
~/slow-32/emulator/slow32 -r program.s32x

# Step through one instruction at a time
~/slow-32/emulator/slow32 -s program.s32x

# Set a breakpoint
~/slow-32/emulator/slow32 -b 0x100 program.s32x

# Watch memory accesses
~/slow-32/emulator/slow32 -w 0x1000-0x2000 program.s32x

# Limit execution cycles (catch infinite loops)
~/slow-32/emulator/slow32 -c 1000 program.s32x
```

## Analyzing Binaries

```bash
# Disassemble executable
~/slow-32/tools/slow32dis program.s32x

# Examine object file
~/slow-32/tools/s32-objdump program.s32o

# Examine executable structure
~/slow-32/tools/s32-exedump program.s32x
```

## Example: Hello World in C

```c
// hello.c
void debug_putchar(int c) {
    // Assembly: debug r3
    asm volatile("debug %0" : : "r"(c));
}

void print(const char* str) {
    while (*str) {
        debug_putchar(*str++);
    }
}

int main() {
    print("Hello, SLOW-32 from C!\n");
    return 0;
}
```

Compile and run:
```bash
./slow32-compile.sh hello.c
~/slow-32/emulator/slow32 hello.s32x
```

## Important Notes

1. **Always use -O1 or -O2** - Unoptimized code exhausts registers
2. **Link order matters** - crt0.s32o must be first
3. **Debug instruction** - Currently the only I/O mechanism
4. **Memory layout**:
   - Code: 0x00000000 - 0x000FFFFF (execute-only)
   - Data: 0x00100000 - 0x0FFFFFFF (read/write)
   - Stack: Starts at 0x0FFFFFF0, grows down

## Next Steps

- Read the [Programmer's Guide](20-programmers-guide.md) for detailed ABI information
- Check the [Instruction Set Reference](25-instruction-set.md) for assembly programming
- See [Examples](../examples/) for more sample programs
- Review [Known Issues](91-known-issues.md) for current limitations

## Troubleshooting

### Common Issues

**Problem**: "Register allocation failed"
**Solution**: Use -O1 or -O2 optimization level

**Problem**: Infinite loop in emulator
**Solution**: Use -c flag to limit cycles: `slow32 -c 10000 program.s32x`

**Problem**: Segmentation fault in program
**Solution**: Check stack usage - default stack is limited

**Problem**: Undefined symbol during linking
**Solution**: Ensure intrinsics.s32o is linked for memcpy/memset

## Getting Help

- Check the [FAQ](92-faq.md)
- Review [troubleshooting guide](93-troubleshooting.md)
- See implementation notes in private repositories