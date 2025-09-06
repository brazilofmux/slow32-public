# Quickstart

Want to see SLOW-32 in action? Here's the fastest path to running code.

## Prerequisites
- Built SLOW-32 toolchain (see [Getting Started](01-getting-started.md))

## Run Your First Program

```bash
# 1. Get the example
cp examples/hello-debug.s hello.s

# 2. Assemble
~/slow-32/assembler/slow32asm -o hello.s32o hello.s

# 3. Link
~/slow-32/linker/s32-ld -o hello.s32x \
    ~/slow-32/runtime/crt0.s32o hello.s32o

# 4. Run
~/slow-32/emulator/slow32 hello.s32x
```

You should see:
```
Hello, SLOW-32!
```

## What Just Happened?

1. **hello.s** - Assembly source with DEBUG instructions to output characters
2. **hello.s32o** - Object file with machine code and relocations
3. **hello.s32x** - Executable with resolved addresses
4. **slow32** - Emulator that executes the program

## Try Debug Mode

See every instruction as it executes:
```bash
~/slow-32/emulator/slow32 -t hello.s32x | head -20
```

## Next Steps
- [Getting Started Guide](01-getting-started.md) - Full setup instructions
- [Programmer's Guide](20-programmers-guide.md) - Write your own programs
- [Examples](../examples/) - More sample code