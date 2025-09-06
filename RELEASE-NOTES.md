# SLOW-32 Release Notes

## Version 1.0.0 (September 5, 2025)

First public release of the SLOW-32 architecture and toolchain.

### Included Components

#### Core Toolchain Binaries
- `slow32asm` - Assembler
- `s32-ld` - Linker  
- `slow32` - Reference emulator with debugging
- `slow32-fast` - Performance emulator (~350M inst/sec)
- `slow32dis` - Disassembler
- `s32-objdump` - Object file analyzer
- `s32-exedump` - Executable analyzer

#### LLVM Backend
- Complete SLOW32 target implementation
- Clang integration for C/C++ compilation
- Support for optimization levels -O1, -O2, -O3

#### Runtime Libraries
- `crt0.s32o` - C runtime startup
- `intrinsics.s32o` - Built-in functions (memcpy, memset)
- `debug_char.s32o` - Debug output support

### Architecture Features

#### Implemented ✅
- 32-bit RISC instruction set
- 32 general-purpose registers
- Load/store architecture
- Function calls with standard ABI
- Stack management
- Global variables and arrays
- Switch statements with jump tables
- Varargs support
- Basic 64-bit integer support (ADD/SUB)
- DEBUG instruction for output

#### Partially Implemented ⚠️
- 64-bit multiply/divide (via libcalls)
- Inline assembly (basic support)
- Large struct passing

#### Not Implemented ❌
- Floating point operations
- SIMD/Vector instructions
- Hardware interrupts
- Memory-mapped I/O (planned)
- System calls (planned)

### Known Limitations

1. **Requires optimization** - Must use -O1 or higher with Clang
2. **No standard library** - Freestanding environment only
3. **DEBUG-only I/O** - No stdin, limited output
4. **Single-threaded** - No atomic operations
5. **Fixed memory layout** - Code at 0x0, data at 0x100000

### Installation

See [Installation Guide](docs/02-installation.md) for detailed instructions.

Binary distributions available for:
- Linux x86_64 (Ubuntu 20.04+, Debian 11+)
- Other platforms: Build from source

### Documentation

Comprehensive documentation included:
- Architecture rationale and philosophy
- Complete instruction set reference
- Programmer's guide with ABI details
- LLVM backend integration guide
- Example programs

### Testing

All regression tests passing:
- Basic arithmetic and logic
- Function calls and recursion
- Arrays and pointers
- Control flow and loops
- Variable arguments
- Switch statements

### Contributors

- S. Dennis - Architecture design and implementation
- Claude - Documentation and testing assistance

### Future Plans

Near-term (v1.1):
- Complete 64-bit integer support
- Basic I/O beyond DEBUG
- Memory-mapped I/O regions

Medium-term (v1.2):
- Floating point support (soft-float)
- TRAP instruction for system calls
- Source-level debugging information

Long-term (v2.0):
- SIMD exploration
- JIT compilation
- Multi-core support (maybe)

### Acknowledgments

Thanks to:
- LLVM project for the compiler infrastructure
- RISC-V project for design inspiration
- The computer architecture education community

### License

MIT License for documentation and examples.
Apache 2.0 with LLVM exceptions for the LLVM backend.
See LICENSE files in respective directories.

### Reporting Issues

Please report issues on GitHub with:
- Tool version
- Operating system
- Minimal test case
- Error messages

### Getting Started

```bash
# Quick test
cat > test.s << EOF
.text
.globl _start
_start:
    li r1, 42
    halt
EOF

slow32asm -o test.o test.s
s32-ld -o test.exe crt0.s32o test.o
slow32 test.exe
echo $?  # Should print 42
```

Welcome to SLOW-32!