# DEPRECATED - Historical Standalone Compiler

The original standalone SLOW-32 compiler (formerly `tools/compiler-deprecated/`, removed in cd1a122b) was the one that attempted to parse x86-targeted LLVM IR and translate it to SLOW-32 assembly.

## Why It's Deprecated

This compiler was a valiant effort that implemented:

- SSA/PHI node handling
- Register allocation and spilling
- Basic instruction translation

However, it had fundamental limitations:

- **Varargs support was impossible** - The x86 ABI assumptions in the IR couldn't be properly translated
- Architecture-specific assumptions in the IR made correct translation increasingly difficult
- It was essentially fighting against LLVM rather than working with it

## The Solution

This led to developing a proper LLVM backend for SLOW-32, which:

- Generates correct SLOW-32 code directly from LLVM
- Properly handles all C features including varargs
- Integrates with clang as a native target: `-target slow32-unknown-none`

## Current Status

**DO NOT USE THIS COMPILER FOR NEW WORK**

Use the modern compilation flow:
```bash
~/llvm-project/build/bin/clang -target slow32-unknown-none -S -emit-llvm -O2 -Iruntime/include program.c -o program.ll
~/llvm-project/build/bin/llc -mtriple=slow32-unknown-none program.ll -o program.s
./tools/assembler/slow32asm program.s program.s32o
./tools/linker/s32-ld -o program.s32x runtime/crt0.s32o program.s32o runtime/libc_debug.s32a runtime/libs32.s32a
./tools/emulator/slow32 program.s32x
```

It is gone from the tree; git history has it.
