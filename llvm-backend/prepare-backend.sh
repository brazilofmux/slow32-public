#!/bin/bash
# Script to extract SLOW32 backend from private LLVM tree and prepare for distribution
# Run this from ~/llvm-project to create a distributable backend

set -e

# Check we're in llvm-project directory
if [ ! -d "llvm/lib/Target/SLOW32" ]; then
    echo "Error: Run this from ~/llvm-project directory"
    echo "Looking for llvm/lib/Target/SLOW32"
    exit 1
fi

OUTPUT_DIR="/tmp/slow32-llvm-backend"
PUBLIC_DIR="$HOME/slow32-public/llvm-backend"

echo "Preparing SLOW32 backend for distribution..."

# Clean previous output
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"/{SLOW32,patches,clang-support,tests}

# Copy the backend files
echo "Copying SLOW32 backend..."
cp -r llvm/lib/Target/SLOW32/* "$OUTPUT_DIR/SLOW32/"

# Remove any private or build files
find "$OUTPUT_DIR/SLOW32" -name "*.o" -delete
find "$OUTPUT_DIR/SLOW32" -name "*.d" -delete
find "$OUTPUT_DIR/SLOW32" -name ".*.swp" -delete

# Generate LLVM core patch
echo "Generating LLVM core patches..."
cat > "$OUTPUT_DIR/patches/llvm-core.patch" << 'EOF'
diff --git a/llvm/CMakeLists.txt b/llvm/CMakeLists.txt
--- a/llvm/CMakeLists.txt
+++ b/llvm/CMakeLists.txt
@@ -450,6 +450,7 @@ set(LLVM_ALL_TARGETS
   RISCV
   Sparc
   SystemZ
+  SLOW32
   VE
   WebAssembly
   X86
diff --git a/llvm/lib/Target/CMakeLists.txt b/llvm/lib/Target/CMakeLists.txt
--- a/llvm/lib/Target/CMakeLists.txt
+++ b/llvm/lib/Target/CMakeLists.txt
@@ -20,6 +20,7 @@ add_subdirectory(NVPTX)
 add_subdirectory(PowerPC)
 add_subdirectory(RISCV)
 add_subdirectory(Sparc)
+add_subdirectory(SLOW32)
 add_subdirectory(SystemZ)
 add_subdirectory(VE)
 add_subdirectory(WebAssembly)
diff --git a/llvm/lib/Support/Triple.cpp b/llvm/lib/Support/Triple.cpp
--- a/llvm/lib/Support/Triple.cpp
+++ b/llvm/lib/Support/Triple.cpp
@@ -45,6 +45,7 @@ public:
     ppc64le,        // PPC64LE: powerpc64le
     r600,           // R600: AMD GPUs HD2XXX - HD6XXX
     riscv32,        // RISC-V (32-bit): riscv32
+    slow32,         // SLOW-32 (32-bit): slow32
     sparc,          // Sparc: sparc
   };
 
@@ -200,6 +201,7 @@ StringRef Triple::getArchTypeName(ArchType Kind) {
   case ppc64le:        return "powerpc64le";
   case r600:           return "r600";
   case riscv32:        return "riscv32";
+  case slow32:         return "slow32";
   case sparc:          return "sparc";
   }
 }
@@ -500,6 +502,7 @@ static Triple::ArchType parseArch(StringRef ArchName) {
     .Case("r600", Triple::r600)
     .Case("riscv32", Triple::riscv32)
     .Case("riscv64", Triple::riscv64)
+    .Case("slow32", Triple::slow32)
     .Case("sparc", Triple::sparc)
     .Default(Triple::UnknownArch);
 }
@@ -900,6 +903,7 @@ Triple::ObjectFormatType Triple::getDefaultFormat(const Triple &T) {
   case Triple::riscv32:
   case Triple::riscv64:
   case Triple::sparc:
+  case Triple::slow32:
   case Triple::sparcv9:
   case Triple::systemz:
   case Triple::x86:
diff --git a/llvm/include/llvm/BinaryFormat/ELF.h b/llvm/include/llvm/BinaryFormat/ELF.h
--- a/llvm/include/llvm/BinaryFormat/ELF.h
+++ b/llvm/include/llvm/BinaryFormat/ELF.h
@@ -300,6 +300,7 @@ enum {
   EM_RISCV = 243,       // RISC-V
   EM_LANAI = 244,       // Lanai 32-bit processor
   EM_BPF = 247,         // Linux kernel bpf virtual machine
+  EM_SLOW32 = 0x5332,   // SLOW-32 processor ('S2' in hex)
   EM_VE = 251,          // NEC SX-Aurora VE
   EM_CSKY = 252,        // C-SKY 32-bit processor
   EM_LOONGARCH = 258,   // LoongArch
EOF

# Generate Clang support patch
echo "Generating Clang patches..."
cat > "$OUTPUT_DIR/patches/clang-target.patch" << 'EOF'
diff --git a/clang/lib/Basic/Targets.cpp b/clang/lib/Basic/Targets.cpp
--- a/clang/lib/Basic/Targets.cpp
+++ b/clang/lib/Basic/Targets.cpp
@@ -45,6 +45,7 @@
 #include "Targets/RISCV.h"
 #include "Targets/SPIR.h"
 #include "Targets/Sparc.h"
+#include "Targets/SLOW32.h"
 #include "Targets/SystemZ.h"
 #include "Targets/TCE.h"
 #include "Targets/VE.h"
@@ -600,6 +601,9 @@ std::unique_ptr<TargetInfo> AllocateTarget(const llvm::Triple &Triple,
   case llvm::Triple::sparcel:
     return std::make_unique<SparcelTargetInfo>(Triple, Opts);
 
+  case llvm::Triple::slow32:
+    return std::make_unique<SLOW32TargetInfo>(Triple, Opts);
+
   case llvm::Triple::systemz:
     return std::make_unique<SystemZTargetInfo>(Triple, Opts);
 
diff --git a/clang/lib/Basic/Targets/CMakeLists.txt b/clang/lib/Basic/Targets/CMakeLists.txt
--- a/clang/lib/Basic/Targets/CMakeLists.txt
+++ b/clang/lib/Basic/Targets/CMakeLists.txt
@@ -20,6 +20,7 @@ add_clang_library(clangBasicTargets
   RISCV.cpp
   SPIR.cpp
   Sparc.cpp
+  SLOW32.cpp
   SystemZ.cpp
   TCE.cpp
   VE.cpp
EOF

# Create Clang target support files
echo "Creating Clang target support files..."

cat > "$OUTPUT_DIR/clang-support/SLOW32.h" << 'EOF'
//===--- SLOW32.h - Declare SLOW32 target feature support ------*- C++ -*-===//
//
// SLOW32 Target Info for Clang
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_BASIC_TARGETS_SLOW32_H
#define LLVM_CLANG_LIB_BASIC_TARGETS_SLOW32_H

#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "llvm/Support/Compiler.h"
#include "llvm/TargetParser/Triple.h"

namespace clang {
namespace targets {

class LLVM_LIBRARY_VISIBILITY SLOW32TargetInfo : public TargetInfo {
public:
  SLOW32TargetInfo(const llvm::Triple &Triple, const TargetOptions &Opts)
      : TargetInfo(Triple) {
    NoAsmVariants = true;
    LongWidth = LongAlign = 32;
    PointerWidth = PointerAlign = 32;
    IntPtrType = SignedInt;
    PtrDiffType = SignedInt;
    SizeType = UnsignedInt;
    
    // SLOW32 data layout
    resetDataLayout("e-m:e-p:32:32-i32:32-i64:32-f32:32-f64:32-"
                   "v64:32-v128:32-a:0:32-n32-S32");
  }

  void getTargetDefines(const LangOptions &Opts,
                       MacroBuilder &Builder) const override;
  
  ArrayRef<Builtin::Info> getTargetBuiltins() const override { 
    return ArrayRef<Builtin::Info>(); 
  }

  BuiltinVaListKind getBuiltinVaListKind() const override {
    return TargetInfo::CharPtrBuiltinVaList;
  }

  ArrayRef<const char *> getGCCRegNames() const override;
  
  ArrayRef<TargetInfo::GCCRegAlias> getGCCRegAliases() const override {
    return ArrayRef<TargetInfo::GCCRegAlias>();
  }

  bool validateAsmConstraint(const char *&Name,
                            TargetInfo::ConstraintInfo &Info) const override {
    switch (*Name) {
    default:
      return false;
    case 'r': // General register
      Info.setAllowsRegister();
      return true;
    }
  }

  std::string_view getClobbers() const override { return ""; }
};

} // namespace targets
} // namespace clang

#endif // LLVM_CLANG_LIB_BASIC_TARGETS_SLOW32_H
EOF

cat > "$OUTPUT_DIR/clang-support/SLOW32.cpp" << 'EOF'
//===--- SLOW32.cpp - Implement SLOW32 target feature support ------------===//
//
// SLOW32 Target Info for Clang
//
//===----------------------------------------------------------------------===//

#include "SLOW32.h"
#include "clang/Basic/MacroBuilder.h"

using namespace clang;
using namespace clang::targets;

void SLOW32TargetInfo::getTargetDefines(const LangOptions &Opts,
                                        MacroBuilder &Builder) const {
  Builder.defineMacro("__slow32__");
  Builder.defineMacro("__SLOW32__");
}

ArrayRef<const char *> SLOW32TargetInfo::getGCCRegNames() const {
  static const char *const GCCRegNames[] = {
    "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",
    "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15",
    "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",
    "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31"
  };
  return llvm::ArrayRef(GCCRegNames);
}
EOF

# Create test files
echo "Creating test files..."

cat > "$OUTPUT_DIR/tests/basic.ll" << 'EOF'
; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s

; CHECK-LABEL: add_test:
define i32 @add_test(i32 %a, i32 %b) {
  ; CHECK: add r1, r3, r4
  %sum = add i32 %a, %b
  ret i32 %sum
}

; CHECK-LABEL: main:
define i32 @main() {
  ; CHECK: li r1, 42
  ret i32 42
}
EOF

cat > "$OUTPUT_DIR/tests/hello.c" << 'EOF'
// Test C compilation with Clang
// clang --target=slow32-unknown-none -S hello.c

int main() {
    return 42;
}
EOF

# Create CMakeLists.txt for the SLOW32 directory if not present
if [ ! -f "$OUTPUT_DIR/SLOW32/CMakeLists.txt" ]; then
    echo "Creating CMakeLists.txt..."
    cat > "$OUTPUT_DIR/SLOW32/CMakeLists.txt" << 'EOF'
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
EOF
fi

echo ""
echo "SLOW32 backend prepared in: $OUTPUT_DIR"
echo ""
echo "To use:"
echo "1. Copy $OUTPUT_DIR/SLOW32 to llvm/lib/Target/"
echo "2. Apply patches from $OUTPUT_DIR/patches/"
echo "3. Copy Clang support files from $OUTPUT_DIR/clang-support/"
echo "4. Build LLVM with -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=SLOW32"
echo ""
echo "Or copy everything to the public repository:"
echo "cp -r $OUTPUT_DIR/* $PUBLIC_DIR/"