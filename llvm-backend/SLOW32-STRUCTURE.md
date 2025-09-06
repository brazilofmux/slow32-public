# SLOW32 Backend Directory Structure

The SLOW32 backend directory that you'll copy to `llvm/lib/Target/SLOW32/` should contain:

## Required Files

### TableGen Files (Target Description)
- `SLOW32.td` - Main target description
- `SLOW32RegisterInfo.td` - Register definitions
- `SLOW32InstrInfo.td` - Instruction definitions
- `SLOW32CallingConv.td` - Calling conventions
- `SLOW32InstrFormats.td` - Instruction format definitions

### C++ Implementation Files
- `SLOW32TargetMachine.cpp/h` - Target machine configuration
- `SLOW32Subtarget.cpp/h` - Subtarget features
- `SLOW32ISelLowering.cpp/h` - Custom lowering code
- `SLOW32ISelDAGToDAG.cpp` - DAG to DAG instruction selection
- `SLOW32InstrInfo.cpp/h` - Instruction information
- `SLOW32RegisterInfo.cpp/h` - Register information
- `SLOW32FrameLowering.cpp/h` - Stack frame handling
- `SLOW32AsmPrinter.cpp` - Assembly printer
- `SLOW32MCInstLower.cpp/h` - Lower MachineInstr to MCInst
- `SLOW32TargetObjectFile.cpp/h` - Object file specifics

### Build Files
- `CMakeLists.txt` - Main CMake configuration
- `MCTargetDesc/CMakeLists.txt` - MC layer build
- `TargetInfo/CMakeLists.txt` - Target info build

### Subdirectories
- `MCTargetDesc/` - Machine code target description
  - `SLOW32MCTargetDesc.cpp/h`
  - `SLOW32MCAsmInfo.cpp/h`
  - `SLOW32MCExpr.cpp/h`
- `TargetInfo/` - Target information
  - `SLOW32TargetInfo.cpp/h`

## File Templates

### CMakeLists.txt (Main)
```cmake
set(LLVM_TARGET_DEFINITIONS SLOW32.td)

tablegen(LLVM SLOW32GenRegisterInfo.inc -gen-register-info)
tablegen(LLVM SLOW32GenInstrInfo.inc -gen-instr-info)
tablegen(LLVM SLOW32GenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM SLOW32GenDAGISel.inc -gen-dag-isel)
tablegen(LLVM SLOW32GenCallingConv.inc -gen-callingconv)
tablegen(LLVM SLOW32GenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(SLOW32CommonTableGen)

add_llvm_target(SLOW32CodeGen
  SLOW32AsmPrinter.cpp
  SLOW32FrameLowering.cpp
  SLOW32InstrInfo.cpp
  SLOW32ISelDAGToDAG.cpp
  SLOW32ISelLowering.cpp
  SLOW32MCInstLower.cpp
  SLOW32RegisterInfo.cpp
  SLOW32Subtarget.cpp
  SLOW32TargetMachine.cpp
  SLOW32TargetObjectFile.cpp

  LINK_COMPONENTS
  Analysis
  AsmPrinter
  CodeGen
  Core
  MC
  SLOW32Desc
  SLOW32Info
  SelectionDAG
  Support
  Target

  ADD_TO_COMPONENT
  SLOW32
)

add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
```

### MCTargetDesc/CMakeLists.txt
```cmake
add_llvm_library(LLVMSLOW32Desc
  SLOW32MCAsmInfo.cpp
  SLOW32MCTargetDesc.cpp
  SLOW32MCExpr.cpp

  LINK_COMPONENTS
  MC
  SLOW32Info
  Support

  ADD_TO_COMPONENT
  SLOW32
)
```

### TargetInfo/CMakeLists.txt
```cmake
add_llvm_library(LLVMSLOW32Info
  SLOW32TargetInfo.cpp

  LINK_COMPONENTS
  Support

  ADD_TO_COMPONENT
  SLOW32
)
```

## Note on Distribution

The actual SLOW32 backend source files are not included in this public repository. They will be:

1. **Distributed as a separate archive** - For those with access
2. **Built from specification** - Using the documentation to recreate
3. **Obtained from binary releases** - Pre-built LLVM with SLOW32

The patches and integration instructions in this repository show exactly how to integrate the SLOW32 backend once you have it.