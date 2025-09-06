//===--- SLOW32.h - Declare SLOW32 target feature support ------*- C++ -*-===//
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
public:
  SLOW32TargetInfo(const llvm::Triple &Triple, const TargetOptions &Opts)
      : TargetInfo(Triple) {
    NoAsmVariants = true;
    LongWidth = LongAlign = 32;
    PointerWidth = PointerAlign = 32;
    IntPtrType = SignedInt;
    PtrDiffType = SignedInt;
    SizeType = UnsignedInt;
    
    // SLOW32 data layout string
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
    case 'r': // General purpose register
      Info.setAllowsRegister();
      return true;
    }
  }

  std::string_view getClobbers() const override { return ""; }
  
  bool hasFeature(StringRef Feature) const override { return false; }
};

} // namespace targets
} // namespace clang

#endif // LLVM_CLANG_LIB_BASIC_TARGETS_SLOW32_H