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

  // i32 unsigned division/remainder (hardware has only signed DIV/REM)
  Sink.setLibcallImpl(RTLIB::UDIV_I32, RTLIB::impl___udivsi3);
  Sink.setLibcallImpl(RTLIB::UREM_I32, RTLIB::impl___umodsi3);

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
