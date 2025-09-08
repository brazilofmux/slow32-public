# SLOW-32 Changelog

## v2.0.0 - 2025-09-08

### Major Features
- **MMIO Support**: Full memory-mapped I/O implementation for console operations
  - Ring buffer architecture for efficient data transfer
  - Console input/output with status checking
  - YIELD instruction support for cooperative I/O
  - Automatic heap allocation by linker

### Emulator Improvements
- **Memory-efficient emulator**: Sparse memory management replacing flat allocation
- **Performance optimizations**: Improved instruction dispatch and memory access
- **MMIO integration**: Both slow32 and slow32-fast emulators support MMIO

### Linker Enhancements
- **Intelligent memory layout**: Automatic heap and MMIO region management
- **MMIO configuration**: Support for --enable-mmio flag
- **Memory protection**: Enhanced W^X enforcement

### Documentation
- Updated I/O documentation with MMIO examples
- Roadmap updated with completed features
- Added MMIO usage examples

## Previous Releases

See git history for earlier changes.