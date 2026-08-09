; RUN: llc -mtriple=slow32-unknown-none -mattr=-f < %s | FileCheck --check-prefix=SOFT %s
; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck --check-prefix=HARD %s

; Without +f, FP types are illegal and every operation softens to the
; standard compiler-rt libcalls (users must link a soft-float library).
; With +f (the default) the native f32/f64 instructions are used.
; -mattr=-f previously hung the op legalizer on FP compares (SETCC <->
; SELECT_CC ping-pong on still-legal FP types) and resolved arithmetic to
; null libcalls, emitting an indirect call through an uninitialized
; register.

define float @add32(float %a, float %b) {
; SOFT-LABEL: add32:
; SOFT: jal r31, __addsf3
; HARD-LABEL: add32:
; HARD: fadd.s
; HARD-NOT: __addsf3
entry:
  %r = fadd float %a, %b
  ret float %r
}

define i1 @lt64(double %a, double %b) {
; SOFT-LABEL: lt64:
; SOFT: jal r31, __ltdf2
; HARD-LABEL: lt64:
; HARD: flt.d
; HARD-NOT: __ltdf2
entry:
  %r = fcmp olt double %a, %b
  ret i1 %r
}

define double @cvt(i64 %v) {
; SOFT-LABEL: cvt:
; SOFT: jal r31, __floatundidf
entry:
  %r = uitofp i64 %v to double
  ret double %r
}

define double @mul64(double %a, double %b) {
; SOFT-LABEL: mul64:
; SOFT: jal r31, __muldf3
; HARD-LABEL: mul64:
; HARD: fmul.d
entry:
  %r = fmul double %a, %b
  ret double %r
}
