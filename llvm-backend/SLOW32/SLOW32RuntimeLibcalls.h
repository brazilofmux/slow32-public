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

  // libm, for the llvm.* math intrinsics clang emits when errno is not in
  // play (C++ <cmath>, -fno-math-errno, the optimizer's own rewrites, and
  // fmin/fmax always).
  // Without these an f32 llvm.log failed with "no libcall available for
  // flog" and an llvm.sin.f32 could not be selected at all; plain calls to
  // logf() were never affected.  The runtime ships each of these except
  // exp2 and exp10, which are bound anyway so that using them is a link
  // error naming the function, as for the soft-float builtins above.
  Sink.setLibcallImpl(RTLIB::SIN_F32, RTLIB::impl_sinf);
  Sink.setLibcallImpl(RTLIB::SIN_F64, RTLIB::impl_sin);
  Sink.setLibcallImpl(RTLIB::COS_F32, RTLIB::impl_cosf);
  Sink.setLibcallImpl(RTLIB::COS_F64, RTLIB::impl_cos);
  Sink.setLibcallImpl(RTLIB::TAN_F32, RTLIB::impl_tanf);
  Sink.setLibcallImpl(RTLIB::TAN_F64, RTLIB::impl_tan);
  Sink.setLibcallImpl(RTLIB::ASIN_F32, RTLIB::impl_asinf);
  Sink.setLibcallImpl(RTLIB::ASIN_F64, RTLIB::impl_asin);
  Sink.setLibcallImpl(RTLIB::ACOS_F32, RTLIB::impl_acosf);
  Sink.setLibcallImpl(RTLIB::ACOS_F64, RTLIB::impl_acos);
  Sink.setLibcallImpl(RTLIB::ATAN_F32, RTLIB::impl_atanf);
  Sink.setLibcallImpl(RTLIB::ATAN_F64, RTLIB::impl_atan);
  Sink.setLibcallImpl(RTLIB::ATAN2_F32, RTLIB::impl_atan2f);
  Sink.setLibcallImpl(RTLIB::ATAN2_F64, RTLIB::impl_atan2);
  Sink.setLibcallImpl(RTLIB::SINH_F32, RTLIB::impl_sinhf);
  Sink.setLibcallImpl(RTLIB::SINH_F64, RTLIB::impl_sinh);
  Sink.setLibcallImpl(RTLIB::COSH_F32, RTLIB::impl_coshf);
  Sink.setLibcallImpl(RTLIB::COSH_F64, RTLIB::impl_cosh);
  Sink.setLibcallImpl(RTLIB::TANH_F32, RTLIB::impl_tanhf);
  Sink.setLibcallImpl(RTLIB::TANH_F64, RTLIB::impl_tanh);
  Sink.setLibcallImpl(RTLIB::POW_F32, RTLIB::impl_powf);
  Sink.setLibcallImpl(RTLIB::POW_F64, RTLIB::impl_pow);
  Sink.setLibcallImpl(RTLIB::LOG_F32, RTLIB::impl_logf);
  Sink.setLibcallImpl(RTLIB::LOG_F64, RTLIB::impl_log);
  Sink.setLibcallImpl(RTLIB::LOG2_F32, RTLIB::impl_log2f);
  Sink.setLibcallImpl(RTLIB::LOG2_F64, RTLIB::impl_log2);
  Sink.setLibcallImpl(RTLIB::LOG10_F32, RTLIB::impl_log10f);
  Sink.setLibcallImpl(RTLIB::LOG10_F64, RTLIB::impl_log10);
  Sink.setLibcallImpl(RTLIB::EXP_F32, RTLIB::impl_expf);
  Sink.setLibcallImpl(RTLIB::EXP_F64, RTLIB::impl_exp);
  Sink.setLibcallImpl(RTLIB::EXP2_F32, RTLIB::impl_exp2f);
  Sink.setLibcallImpl(RTLIB::EXP2_F64, RTLIB::impl_exp2);
  Sink.setLibcallImpl(RTLIB::EXP10_F32, RTLIB::impl_exp10f);
  Sink.setLibcallImpl(RTLIB::EXP10_F64, RTLIB::impl_exp10);
  Sink.setLibcallImpl(RTLIB::CBRT_F32, RTLIB::impl_cbrtf);
  Sink.setLibcallImpl(RTLIB::CBRT_F64, RTLIB::impl_cbrt);
  Sink.setLibcallImpl(RTLIB::LDEXP_F32, RTLIB::impl_ldexpf);
  Sink.setLibcallImpl(RTLIB::LDEXP_F64, RTLIB::impl_ldexp);
  Sink.setLibcallImpl(RTLIB::FREXP_F32, RTLIB::impl_frexpf);
  Sink.setLibcallImpl(RTLIB::FREXP_F64, RTLIB::impl_frexp);
  Sink.setLibcallImpl(RTLIB::MODF_F32, RTLIB::impl_modff);
  Sink.setLibcallImpl(RTLIB::MODF_F64, RTLIB::impl_modf);
  // fmin/fmax: clang makes a plain fmax() call llvm.maxnum, which without
  // these was "no libcall available for fmaxnum"
  Sink.setLibcallImpl(RTLIB::FMIN_F32, RTLIB::impl_fminf);
  Sink.setLibcallImpl(RTLIB::FMIN_F64, RTLIB::impl_fmin);
  Sink.setLibcallImpl(RTLIB::FMAX_F32, RTLIB::impl_fmaxf);
  Sink.setLibcallImpl(RTLIB::FMAX_F64, RTLIB::impl_fmax);

  Sink.setLibcallImpl(RTLIB::MEMCPY, RTLIB::impl_memcpy);
  Sink.setLibcallImpl(RTLIB::MEMMOVE, RTLIB::impl_memmove);
  Sink.setLibcallImpl(RTLIB::MEMSET, RTLIB::impl_memset);

  // DWARF EH: a landing pad that only cleans up ends in `resume`, which
  // DwarfEHPrepare lowers to this call (runtime/unwind_asm.s).  Unbound,
  // it came out as a call to an undefined "__unnamed_N".
  Sink.setLibcallImpl(RTLIB::UNWIND_RESUME, RTLIB::impl__Unwind_Resume);
}

} // namespace llvm

#endif
