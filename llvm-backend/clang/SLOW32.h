//===--- SLOW32.h - Declare SLOW32 target feature support -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares SLOW32 TargetInfo objects.
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
  static const char *const GCCRegNames[];

public:
  SLOW32TargetInfo(const llvm::Triple &Triple, const TargetOptions &)
      : TargetInfo(Triple) {
    // Basic sizes - SLOW32 is a 32-bit architecture
    IntWidth = IntAlign = 32;
    LongWidth = LongAlign = 32;
    LongLongWidth = 64;
    LongLongAlign = 32;  // 32-bit alignment for i64 (no native 64-bit support)
    PointerWidth = PointerAlign = 32;
    SuitableAlign = 32;
    
    // Floating point sizes
    FloatWidth = FloatAlign = 32;
    DoubleWidth = 64;
    DoubleAlign = 32;  // 32-bit alignment for doubles (simpler than 64-bit)
    LongDoubleWidth = 64;  // Same as double
    LongDoubleAlign = 32;  // 32-bit alignment
    LongDoubleFormat = &llvm::APFloat::IEEEdouble();
    
    // Standard types
    SizeType = UnsignedInt;
    PtrDiffType = SignedInt;
    IntPtrType = SignedInt;
    IntMaxType = SignedLongLong;
    WCharType = UnsignedInt;
    WIntType = UnsignedInt;
    SigAtomicType = SignedInt;
    
    // No TLS support yet
    TLSSupported = false;
    
    // Data layout string matching SLOW32 backend
    // e = little endian
    // p:32:32 = 32-bit pointers with 32-bit alignment
    // i8:8:32 = i8 has 8-bit alignment, 32-bit preferred
    // i16:16:32 = i16 has 16-bit alignment, 32-bit preferred  
    // i32:32:32 = i32 has 32-bit alignment
    // i64:32:32 = i64 has 32-bit alignment (no native 64-bit support)
    // f32:32:32 = float has 32-bit alignment
    // f64:32:32 = double has 32-bit alignment (simpler than 64-bit)
    // n8:16:32 = native integer widths are 8, 16, 32
    // S32 = stack alignment is 32 bits
    resetDataLayout("e-m:e-p:32:32-i8:8:32-i16:16:32-i32:32:32-i64:32:32-"
                    "f32:32:32-f64:32:32-n8:16:32-S32");
  }

  void getTargetDefines(const LangOptions &Opts,
                        MacroBuilder &Builder) const override;

  llvm::SmallVector<Builtin::InfosShard> getTargetBuiltins() const override {
    // No target-specific builtins yet
    return {};
  }

  bool hasFeature(StringRef Feature) const override {
    return Feature == "slow32";
  }

  ArrayRef<const char *> getGCCRegNames() const override;

  ArrayRef<TargetInfo::GCCRegAlias> getGCCRegAliases() const override {
    // Register aliases for inline assembly
    static const TargetInfo::GCCRegAlias GCCRegAliases[] = {
        {{"zero"}, "r0"},
        {{"rv"}, "r1"},
        {{"t0"}, "r2"},
        {{"a0"}, "r3"},
        {{"a1"}, "r4"},
        {{"a2"}, "r5"},
        {{"a3"}, "r6"},
        {{"a4"}, "r7"},
        {{"a5"}, "r8"},
        {{"a6"}, "r9"},
        {{"a7"}, "r10"},
        {{"sp"}, "r29"},
        {{"fp"}, "r30"},
        {{"lr", "ra"}, "r31"},
    };
    return llvm::ArrayRef(GCCRegAliases);
  }

  bool validateAsmConstraint(const char *&Name,
                             TargetInfo::ConstraintInfo &Info) const override {
    switch (*Name) {
    default:
      return false;
    case 'r': // General purpose register
      Info.setAllowsRegister();
      return true;
    case 'i': // Integer immediate
    case 'n': // Integer constant without relocation
      Info.setRequiresImmediate();
      return true;
    case 'm': // Memory operand
      Info.setAllowsMemory();
      return true;
    }
  }

  std::string_view getClobbers() const override { return ""; }

  BuiltinVaListKind getBuiltinVaListKind() const override {
    return VoidPtrBuiltinVaList;
  }

  bool allowsLargerPreferedTypeAlignment() const override { return false; }
};

} // namespace targets
} // namespace clang

#endif // LLVM_CLANG_LIB_BASIC_TARGETS_SLOW32_H
