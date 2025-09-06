add_llvm_component_library(LLVMSLOW32Info
  SLOW32TargetInfo.cpp

  LINK_COMPONENTS
  MC
  Support

  ADD_TO_COMPONENT
  SLOW32
)