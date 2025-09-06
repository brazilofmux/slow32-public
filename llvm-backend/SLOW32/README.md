# SLOW32 LLVM Backend

This directory contains the complete SLOW32 backend for LLVM 19.0+.

## Installation

1. Copy this entire directory to `llvm/lib/Target/SLOW32/` in your LLVM tree
2. Apply the integration patches from the parent directory
3. Build LLVM with `-DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD="SLOW32"`

See ../INSTALL.md for detailed instructions.

## Contents

- `SLOW32.td` - Main target description
- `SLOW32InstrInfo.td` - Instruction definitions
- `SLOW32RegisterInfo.td` - Register definitions
- `SLOW32CallingConv.td` - Calling conventions
- `*.cpp/h` - C++ implementation files
- `MCTargetDesc/` - Machine code target description
- `TargetInfo/` - Target information

## Architecture Summary

- 32-bit RISC architecture
- 32 general-purpose registers (r0-r31)
- r0 is hardwired to zero
- Fixed 32-bit instruction width
- Load/store architecture
- No condition codes

## Version

Compatible with LLVM 19.0 and later.

## License

Licensed under Apache 2.0 with LLVM exceptions, same as LLVM itself.