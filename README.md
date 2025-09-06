# SLOW-32 Guide

**Application-level assembly, without the hardware baggage.**

SLOW-32 is a fictitious 32-bit ISA and toolchain designed for learning, compilers, and emulation. No modes, no MMU, no paging, no interrupts, no carry flag—just the essentials for writing code that runs in a host-managed sandbox.

- **Toolchain**: clang tweaks → LLVM backend → assembler → linker → two emulators → objdump/exedump/disassembler
- **I/O**: `DEBUG` instruction today; MMIO ring buffers and a TRAP interface on the roadmap
- **Safety**: one W^X line; IST lives as **.s32x metadata**, not memory-mapped
- **Performance**: tight loops can hit ~200–350M instr/s on a modern host (fast emulator); speed isn't the goal, clarity is

## Start here
- **[Installation](docs/02-installation.md)** - Download and install the toolchain
- **[Quickstart](docs/10-quickstart.md)** - Run your first program in 2 minutes
- **[Architecture Rationale](docs/05-architecture-rationale.md)** - Why SLOW-32 exists
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