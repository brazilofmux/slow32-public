; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target
;
; wrong-code: with no powi libcall bound, the legalizer called pow with the exponent
; converted to the base's type, which rounds an odd exponent past 2^24 in a float.

; CHECK-LABEL: pf:
; CHECK: __powisf2
; CHECK-NOT: powf
define float @pf(float %x, i32 %n) nounwind {
  %r = call float @llvm.powi.f32.i32(float %x, i32 %n)
  ret float %r
}

; CHECK-LABEL: pd:
; CHECK: __powidf2
; CHECK-NOT: {{[^_]pow[ \n]}}
define double @pd(double %x, i32 %n) nounwind {
  %r = call double @llvm.powi.f64.i32(double %x, i32 %n)
  ret double %r
}

declare float @llvm.powi.f32.i32(float, i32)
declare double @llvm.powi.f64.i32(double, i32)
