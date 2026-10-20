; RUN: llc -mtriple=slow32-unknown-none -O2 < %s | FileCheck %s
; REQUIRES: slow32-registered-target
;
; The llvm.* math intrinsics clang emits when errno is not in play (C++
; <cmath>, -fno-math-errno) are libm calls.  Unbound, an f32 llvm.log failed
; with "no libcall available for flog" (and fmaxnum, for a plain fmax()
; call), llvm.sin/cos/pow could not be
; selected, and frem was expanded inline into calls to rint and fma, which
; the runtime does not ship; frem is the runtime's fmodf/fmod.

; CHECK-LABEL: f_log:
; CHECK: jal r31, logf
define float @f_log(float %x) {
  %r = call float @llvm.log.f32(float %x)
  ret float %r
}

; CHECK-LABEL: d_exp:
; CHECK: jal r31, exp
define double @d_exp(double %x) {
  %r = call double @llvm.exp.f64(double %x)
  ret double %r
}

; CHECK-LABEL: f_sin:
; CHECK: jal r31, sinf
define float @f_sin(float %x) {
  %r = call float @llvm.sin.f32(float %x)
  ret float %r
}

; CHECK-LABEL: d_pow:
; CHECK: jal r31, pow
define double @d_pow(double %x, double %y) {
  %r = call double @llvm.pow.f64(double %x, double %y)
  ret double %r
}

; sin and cos of one value: two calls, not a sincos the runtime lacks
; CHECK-LABEL: f_sincos:
; CHECK-DAG: jal r31, sinf
; CHECK-DAG: jal r31, cosf
; CHECK-NOT: jal r31, sincos
define float @f_sincos(float %x) {
  %a = call float @llvm.sin.f32(float %x)
  %b = call float @llvm.cos.f32(float %x)
  %r = fadd float %a, %b
  ret float %r
}

; CHECK-LABEL: f_rem:
; CHECK: jal r31, fmodf
; CHECK-NOT: frexpf
define float @f_rem(float %x, float %y) {
  %r = frem float %x, %y
  ret float %r
}

; CHECK-LABEL: d_rem:
; CHECK: jal r31, fmod
; CHECK-NOT: rint
define double @d_rem(double %x, double %y) {
  %r = frem double %x, %y
  ret double %r
}

; CHECK-LABEL: d_max:
; CHECK: jal r31, fmax
define double @d_max(double %x, double %y) {
  %r = call double @llvm.maxnum.f64(double %x, double %y)
  ret double %r
}

; CHECK-LABEL: f_min:
; CHECK: jal r31, fminf
define float @f_min(float %x, float %y) {
  %r = call float @llvm.minnum.f32(float %x, float %y)
  ret float %r
}

declare double @llvm.maxnum.f64(double, double)
declare float @llvm.minnum.f32(float, float)
declare float @llvm.log.f32(float)
declare double @llvm.exp.f64(double)
declare float @llvm.sin.f32(float)
declare float @llvm.cos.f32(float)
declare double @llvm.pow.f64(double, double)
