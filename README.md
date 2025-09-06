# SLOW-32 Architecture

**Application-level assembly, without the hardware baggage.**

SLOW-32 is a minimalist 32-bit RISC architecture designed for learning, compilers, and emulation. No modes, no MMU, no paging, no interrupts, no carry flag—just the essentials for writing code that runs in a host-managed sandbox.

## Quick Start

```bash
# Build the toolchain
cd tools && make && cd ..

# Run your first program
echo '.globl _start; _start: addi r1,r0,72; debug r1; halt' > test.s
tools/assembler/slow32asm -o test.s32o test.s
tools/linker/s32-ld -o test.s32x test.s32o
tools/emulator/slow32 test.s32x  # Prints 'H'
```

See [QUICK-START.md](QUICK-START.md) for detailed instructions.

## Why SLOW-32?

- **Clarity over legacy:** Fixed-width instructions, simple register file, straightforward memory model
- **Predictability over complexity:** No hidden state machines, no interrupts, no undefined behavior  
- **Speed through simplicity:** ~350M instructions/sec on modern hardware (fast emulator)
- **Crash-safe by design:** Programs run in isolated sandboxes; crashes don't affect the host

## Start here
- **[Quick Start Guide](QUICK-START.md)** - Build and run in 5 minutes
- **[Architecture Rationale](docs/05-architecture-rationale.md)** - Why SLOW-32 exists  
- **[Programmer's Guide](docs/20-programmers-guide.md)** - Write SLOW-32 code
- **[Examples](examples/)** - Working code with explanations

## Documentation

### Essential Guides
- **[Installation](docs/02-installation.md)** - Download and install binaries
- **[Getting Started](docs/01-getting-started.md)** - Build from source
- **[Programmer's Guide](docs/20-programmers-guide.md)** - ABI, calling conventions, memory model
- **[Instruction Set](docs/25-instruction-set.md)** - Complete ISA reference
- **[Architecture Rationale](docs/05-architecture-rationale.md)** - Design philosophy

### Toolchain
- **[Toolchain Overview](docs/15-toolchain-overview.md)** - All tools explained
- **[LLVM Backend](docs/35-llvm-backend.md)** - Compiler details and usage
- **[LLVM Integration](llvm-backend/)** - Adding SLOW32 to LLVM/Clang

### Reference
- **[Known Limitations](docs/91-known-limitations.md)** - Current constraints and workarounds
- **[Roadmap](docs/90-roadmap.md)** - Future plans

## Status

**Working:**
- Complete toolchain (assembler, linker, emulator)
- LLVM backend with Clang support
- 32-bit arithmetic and logic
- Function calls and stack management
- Global variables and arrays
- Basic 64-bit integer support
- Varargs and switch statements

**In Progress:**
- Full 64-bit arithmetic
- I/O beyond DEBUG instruction
- Soft-float library

**Planned:**
- MMIO and TRAP interfaces
- Floating-point support
- Source-level debugging

## License
MIT (docs/examples). See individual subprojects for their licenses.