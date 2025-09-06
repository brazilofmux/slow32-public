# Quick Start Guide

Get SLOW-32 running in under 5 minutes.

## Prerequisites

- GCC or Clang compiler
- Make
- Basic Unix tools (Linux, macOS, WSL)

## Building the Toolchain

```bash
# Clone the repository
git clone https://github.com/yourusername/slow32-public.git
cd slow32-public

# Build all tools
cd tools
make
cd ..
```

That's it! The tools are now built in their respective directories.

## Your First Program

Create a file `hello.s`:

```asm
        .section .text
        .globl _start
_start:
        # Print 'H'
        addi r1, r0, 72
        debug r1
        
        # Print 'i'
        addi r1, r0, 105
        debug r1
        
        # Print newline
        addi r1, r0, 10
        debug r1
        
        # Exit
        addi r1, r0, 0
        halt
```

Build and run:

```bash
# Assemble
tools/assembler/slow32asm -o hello.s32o hello.s

# Link
tools/linker/s32-ld -o hello.s32x hello.s32o

# Run
tools/emulator/slow32 hello.s32x
```

You should see "Hi" printed!

## Using C with LLVM (Advanced)

First, build LLVM with the SLOW-32 backend (see [llvm-backend/README.md](llvm-backend/README.md) for full instructions).

Once you have LLVM built with SLOW-32 support:

```bash
# Write C code
cat > hello.c << 'EOF'
int main() {
    return 42;
}
EOF

# Compile to SLOW-32
clang --target=slow32-unknown-none -S -emit-llvm hello.c
llc -mtriple=slow32-unknown-none hello.ll -o hello.s

# Assemble and link with runtime
tools/assembler/slow32asm -o hello.s32o hello.s
tools/linker/s32-ld -o hello.s32x \
    runtime/crt0.s32o hello.s32o runtime/intrinsics.s32o

# Run
tools/emulator/slow32-fast hello.s32x
```

## Tools Overview

| Tool | Purpose | Usage |
|------|---------|-------|
| `slow32asm` | Assembler | `slow32asm -o output.s32o input.s` |
| `s32-ld` | Linker | `s32-ld -o output.s32x input.s32o ...` |
| `slow32` | Debug emulator | `slow32 program.s32x` |
| `slow32-fast` | Fast emulator | `slow32-fast program.s32x` |
| `slow32dis` | Disassembler | `slow32dis program.s32x` |
| `s32-objdump` | Object inspector | `s32-objdump file.s32o` |

## Next Steps

- Read the [Programmer's Guide](docs/20-programmers-guide.md)
- Study the [Instruction Set](docs/25-instruction-set.md)
- Explore the [examples](examples/) directory
- Set up the [LLVM backend](llvm-backend/README.md)

## Troubleshooting

**Build fails with missing headers**
- Make sure you're in the `tools` directory when running `make`

**Assembler says "Unknown instruction"**
- Check the [Instruction Set](docs/25-instruction-set.md)
- Remember: no `li` pseudo-instruction yet, use `addi rd, r0, imm`

**Linker produces no output**
- The linker is silent on success
- Check that the .s32x file was created

**Program doesn't print anything**
- Use `debug r1` not just `debug`
- Make sure r1 contains the ASCII value

**Exit code is wrong**
- Return value goes in r1 before `halt`