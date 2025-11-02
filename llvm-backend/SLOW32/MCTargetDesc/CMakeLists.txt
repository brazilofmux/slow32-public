add_llvm_component_library(LLVMSLOW32Desc
  SLOW32AsmBackend.cpp
  SLOW32ELFObjectWriter.cpp
  SLOW32InstPrinter.cpp
  SLOW32MCAsmInfo.cpp
  SLOW32MCCodeEmitter.cpp
  SLOW32MCExpr.cpp
  SLOW32MCTargetDesc.cpp

  LINK_COMPONENTS
  MC
  SLOW32Info
  Support

  ADD_TO_COMPONENT
  SLOW32
  )
