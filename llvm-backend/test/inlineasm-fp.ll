; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; f32 lives in a single GPR on this target, so an "r" inline-asm constraint must
; accept a float operand instead of failing register allocation.

define float @f(float %x) {
; CHECK-LABEL: f:
; CHECK: fadd.s r{{[0-9]+}}, r{{[0-9]+}}, r{{[0-9]+}}
  %r = call float asm "fadd.s $0, $1, $1", "=r,r"(float %x)
  ret float %r
}
