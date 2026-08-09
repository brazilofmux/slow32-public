//===-- SLOW32RuntimeLibcalls.h - Shared libcall table --------------------===//
//
// Single source of truth for SLOW32 runtime libcall bindings. Both
// TargetLowering::setLibcallImpl (makeLibCall path) and
// Subtarget::initLibcallLoweringInfo (DAG.getLibcalls path) must stay in
// lockstep while LLVM's RuntimeLibcalls API is transitional.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_SLOW32_SLOW32RUNTIMELIBCALLS_H
#define LLVM_LIB_TARGET_SLOW32_SLOW32RUNTIMELIBCALLS_H

#include "llvm/IR/RuntimeLibcalls.h"

namespace llvm {

/// Apply the SLOW32 libcall → implementation map to any object that provides
/// `setLibcallImpl(RTLIB::Libcall, RTLIB::LibcallImpl)`.
template <typename LibcallSink>
static inline void setSLOW32LibcallImpls(LibcallSink &Sink) {
  // i64 division/remainder
  Sink.setLibcallImpl(RTLIB::SDIV_I64, RTLIB::impl___divdi3);
  Sink.setLibcallImpl(RTLIB::UDIV_I64, RTLIB::impl___udivdi3);
  Sink.setLibcallImpl(RTLIB::SREM_I64, RTLIB::impl___moddi3);
  Sink.setLibcallImpl(RTLIB::UREM_I64, RTLIB::impl___umoddi3);

  // i32 mul / signed div when -mattr=-m expands them to libcalls
  Sink.setLibcallImpl(RTLIB::MUL_I32, RTLIB::impl___mulsi3);
  Sink.setLibcallImpl(RTLIB::SDIV_I32, RTLIB::impl___divsi3);
  Sink.setLibcallImpl(RTLIB::SREM_I32, RTLIB::impl___modsi3);
  // i32 unsigned division/remainder (hardware has only signed DIV/REM)
  Sink.setLibcallImpl(RTLIB::UDIV_I32, RTLIB::impl___udivsi3);
  Sink.setLibcallImpl(RTLIB::UREM_I32, RTLIB::impl___umodsi3);

  // Soft-float builtins for -mattr=-f cores (slow32-minimal). The compiler
  // must emit the standard compiler-rt names so a missing implementation
  // surfaces as an honest link error — without these registrations the
  // expanded FP ops resolved to null libcalls and llc emitted an indirect
  // call through an uninitialized register while still exiting 0.
  // NOTE: the slow-32 runtime does not ship these yet; -f users must link
  // a soft-float library.
  Sink.setLibcallImpl(RTLIB::ADD_F32, RTLIB::impl___addsf3);
  Sink.setLibcallImpl(RTLIB::SUB_F32, RTLIB::impl___subsf3);
  Sink.setLibcallImpl(RTLIB::MUL_F32, RTLIB::impl___mulsf3);
  Sink.setLibcallImpl(RTLIB::DIV_F32, RTLIB::impl___divsf3);
  Sink.setLibcallImpl(RTLIB::ADD_F64, RTLIB::impl___adddf3);
  Sink.setLibcallImpl(RTLIB::SUB_F64, RTLIB::impl___subdf3);
  Sink.setLibcallImpl(RTLIB::MUL_F64, RTLIB::impl___muldf3);
  Sink.setLibcallImpl(RTLIB::DIV_F64, RTLIB::impl___divdf3);
  Sink.setLibcallImpl(RTLIB::OEQ_F32, RTLIB::impl___eqsf2);
  Sink.setLibcallImpl(RTLIB::UNE_F32, RTLIB::impl___nesf2);
  Sink.setLibcallImpl(RTLIB::OLT_F32, RTLIB::impl___ltsf2);
  Sink.setLibcallImpl(RTLIB::OLE_F32, RTLIB::impl___lesf2);
  Sink.setLibcallImpl(RTLIB::OGT_F32, RTLIB::impl___gtsf2);
  Sink.setLibcallImpl(RTLIB::OGE_F32, RTLIB::impl___gesf2);
  Sink.setLibcallImpl(RTLIB::UO_F32, RTLIB::impl___unordsf2);
  Sink.setLibcallImpl(RTLIB::OEQ_F64, RTLIB::impl___eqdf2);
  Sink.setLibcallImpl(RTLIB::UNE_F64, RTLIB::impl___nedf2);
  Sink.setLibcallImpl(RTLIB::OLT_F64, RTLIB::impl___ltdf2);
  Sink.setLibcallImpl(RTLIB::OLE_F64, RTLIB::impl___ledf2);
  Sink.setLibcallImpl(RTLIB::OGT_F64, RTLIB::impl___gtdf2);
  Sink.setLibcallImpl(RTLIB::OGE_F64, RTLIB::impl___gedf2);
  Sink.setLibcallImpl(RTLIB::UO_F64, RTLIB::impl___unorddf2);
  Sink.setLibcallImpl(RTLIB::FPEXT_F32_F64, RTLIB::impl___extendsfdf2);
  Sink.setLibcallImpl(RTLIB::FPROUND_F64_F32, RTLIB::impl___truncdfsf2);
  Sink.setLibcallImpl(RTLIB::FPTOSINT_F32_I32, RTLIB::impl___fixsfsi);
  Sink.setLibcallImpl(RTLIB::FPTOSINT_F32_I64, RTLIB::impl___fixsfdi);
  Sink.setLibcallImpl(RTLIB::FPTOSINT_F64_I32, RTLIB::impl___fixdfsi);
  Sink.setLibcallImpl(RTLIB::FPTOSINT_F64_I64, RTLIB::impl___fixdfdi);
  Sink.setLibcallImpl(RTLIB::FPTOUINT_F32_I32, RTLIB::impl___fixunssfsi);
  Sink.setLibcallImpl(RTLIB::FPTOUINT_F32_I64, RTLIB::impl___fixunssfdi);
  Sink.setLibcallImpl(RTLIB::FPTOUINT_F64_I32, RTLIB::impl___fixunsdfsi);
  Sink.setLibcallImpl(RTLIB::FPTOUINT_F64_I64, RTLIB::impl___fixunsdfdi);
  Sink.setLibcallImpl(RTLIB::SINTTOFP_I32_F32, RTLIB::impl___floatsisf);
  Sink.setLibcallImpl(RTLIB::SINTTOFP_I32_F64, RTLIB::impl___floatsidf);
  Sink.setLibcallImpl(RTLIB::SINTTOFP_I64_F32, RTLIB::impl___floatdisf);
  Sink.setLibcallImpl(RTLIB::SINTTOFP_I64_F64, RTLIB::impl___floatdidf);
  Sink.setLibcallImpl(RTLIB::UINTTOFP_I32_F32, RTLIB::impl___floatunsisf);
  Sink.setLibcallImpl(RTLIB::UINTTOFP_I32_F64, RTLIB::impl___floatunsidf);
  Sink.setLibcallImpl(RTLIB::UINTTOFP_I64_F32, RTLIB::impl___floatundisf);
  Sink.setLibcallImpl(RTLIB::UINTTOFP_I64_F64, RTLIB::impl___floatundidf);

  // Soft math helpers the runtime ships
  Sink.setLibcallImpl(RTLIB::REM_F32, RTLIB::impl_fmodf);
  Sink.setLibcallImpl(RTLIB::REM_F64, RTLIB::impl_fmod);
  Sink.setLibcallImpl(RTLIB::RINT_F32, RTLIB::impl_rintf);
  Sink.setLibcallImpl(RTLIB::RINT_F64, RTLIB::impl_rint);
  Sink.setLibcallImpl(RTLIB::NEARBYINT_F32, RTLIB::impl_nearbyintf);
  Sink.setLibcallImpl(RTLIB::NEARBYINT_F64, RTLIB::impl_nearbyint);
  Sink.setLibcallImpl(RTLIB::FLOOR_F32, RTLIB::impl_floorf);
  Sink.setLibcallImpl(RTLIB::FLOOR_F64, RTLIB::impl_floor);
  Sink.setLibcallImpl(RTLIB::CEIL_F32, RTLIB::impl_ceilf);
  Sink.setLibcallImpl(RTLIB::CEIL_F64, RTLIB::impl_ceil);
  Sink.setLibcallImpl(RTLIB::TRUNC_F32, RTLIB::impl_truncf);
  Sink.setLibcallImpl(RTLIB::TRUNC_F64, RTLIB::impl_trunc);
  Sink.setLibcallImpl(RTLIB::ROUND_F32, RTLIB::impl_roundf);
  Sink.setLibcallImpl(RTLIB::ROUND_F64, RTLIB::impl_round);
  Sink.setLibcallImpl(RTLIB::FMA_F32, RTLIB::impl_fmaf);
  Sink.setLibcallImpl(RTLIB::FMA_F64, RTLIB::impl_fma);

  Sink.setLibcallImpl(RTLIB::MEMCPY, RTLIB::impl_memcpy);
  Sink.setLibcallImpl(RTLIB::MEMMOVE, RTLIB::impl_memmove);
  Sink.setLibcallImpl(RTLIB::MEMSET, RTLIB::impl_memset);
}

} // namespace llvm

#endif
