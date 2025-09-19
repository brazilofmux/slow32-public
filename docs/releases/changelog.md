# SLOW-32 Changelog

## 2025-09-08: Ring Buffer MMIO Framework (Design Phase)

### MMIO Architecture Designed
- **Ring buffer pattern**: Inspired by NIC/NVMe controllers
- **Dual rings**: Request ring (CPU→Device), Response ring (Device→CPU)
- **Shared data buffer**: 56KB for bulk transfers
- **Lock-free design**: Producer/consumer indices at 0x10000000

### Components Created (Not Yet Integrated)
- `mmio_ring.h/c`: Core ring buffer implementation
- `mmio_integration.h`: Memory manager hooks
- `mmio_stdio.c`: stdio library using ring buffers
- `test_mmio.c`: Test program for validation

### Design Considerations
- Need to address execution model (polling vs threading)
- Host-side processing strategy required
- Synchronization between emulator and guest code

**Status**: Architecture complete, integration pending design review

## 2025-09-08: Intelligent Memory Layout Management

### Linker Enhancements
- **Automatic compact layouts**: Detects small programs and packs to minimal memory
- **Configurable memory regions**: `--code-size`, `--rodata-size`, `--data-size`, `--stack-size`
- **Ultra-compact mode**: `--compact` flag for 4KB page-based layouts
- **Memory map generation**: `--print-map` creates detailed .map files
- **Smart packing**: `--pack-sections` automatically minimizes gaps

### Results
- **9KB minimum footprint**: "Hello World" in 4KB code + 4KB heap + 1KB stack
- **1.6MB host memory**: Down from 256MB (99.4% reduction)
- **Automatic optimization**: Small programs automatically get compact layouts
- **1980s micro footprint**: Smaller than Commodore 64's 64KB!

### Example
```bash
# Ultra-compact hello world (9KB total)
./linker/s32-ld -o hello.s32x --compact --stack-size 1K hello.s32o

# Generate memory map
./linker/s32-ld -o program.s32x --print-map runtime/crt0.s32o program.s32o
```

## 2025-09-08: W^X Hardware Memory Protection

### Emulator Optimizations
- Removed redundant software W^X checks
- Now relies on hardware mprotect() for memory protection
- Added cached code region pointer for fast instruction fetch
- Performance reporting in slow32 (inst_count, MIPS calculation)

### Results
- **Better protection**: Hardware-enforced W^X via mprotect()
- **Maintained performance**: 45 MIPS (slow32), 220 MIPS (slow32-fast)
- **Cleaner code**: Removed redundant protection checks

## Previous Updates

### Sparse Memory Management
- Replaced flat 256MB allocation with on-demand page allocation
- Only allocates 4KB pages when touched
- 99.4% memory reduction for typical programs

### 64-bit Integer Support
- Full i64 arithmetic, logical, and comparison operations
- Division/remainder via compiler-rt libcalls
- Comprehensive test coverage

### Native LLVM Target
- `--target slow32-unknown-none` support in Clang
- Complete backend with varargs, PHI nodes, switch statements
- Proper ABI compliance