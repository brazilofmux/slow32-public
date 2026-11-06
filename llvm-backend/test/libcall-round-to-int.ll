; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; The rounding conversions are calls: unbound, llc stopped with "no libcall
; available for lround".  Fortran's NINT arrives this way.

declare i32 @llvm.lround.i32.f64(double)
declare i32 @llvm.lround.i32.f32(float)
declare i64 @llvm.llround.i64.f64(double)
declare i32 @llvm.lrint.i32.f64(double)
declare i64 @llvm.llrint.i64.f32(float)
declare double @llvm.roundeven.f64(double)

; CHECK-LABEL: a:
; CHECK: lround
define i32 @a(double %x) {
  %r = call i32 @llvm.lround.i32.f64(double %x)
  ret i32 %r
}
; CHECK-LABEL: b:
; CHECK: lroundf
define i32 @b(float %x) {
  %r = call i32 @llvm.lround.i32.f32(float %x)
  ret i32 %r
}
; CHECK-LABEL: c:
; CHECK: llround
define i64 @c(double %x) {
  %r = call i64 @llvm.llround.i64.f64(double %x)
  ret i64 %r
}
; CHECK-LABEL: d:
; CHECK: lrint
define i32 @d(double %x) {
  %r = call i32 @llvm.lrint.i32.f64(double %x)
  ret i32 %r
}
; CHECK-LABEL: e:
; CHECK: llrintf
define i64 @e(float %x) {
  %r = call i64 @llvm.llrint.i64.f32(float %x)
  ret i64 %r
}
; CHECK-LABEL: f:
; CHECK: roundeven
define double @f(double %x) {
  %r = call double @llvm.roundeven.f64(double %x)
  ret double %r
}
