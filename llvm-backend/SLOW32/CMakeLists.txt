add_llvm_component_group(SLOW32)

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
  SLOW32ISelDAGToDAG.cpp
  SLOW32ISelLowering.cpp
  SLOW32ISelLoweringCombines.cpp
  SLOW32ISelLoweringInlineAsm.cpp
  SLOW32ISelLoweringVarargs.cpp
  SLOW32LoadAddrOpt.cpp
  SLOW32InstrInfo.cpp
  SLOW32MachineFunctionInfo.cpp
  SLOW32MCInstLowering.cpp
  SLOW32RegisterInfo.cpp
  SLOW32Subtarget.cpp
  SLOW32TargetMachine.cpp

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
