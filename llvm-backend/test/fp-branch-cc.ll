; RUN: llc -mtriple=slow32-unknown-none -verify-machineinstrs < %s | FileCheck %s
; A float compare forwarded through memory reaches the branch as BR_CC on the
; floats, which sent the legalizer round for ever.  It must be the compare, then a
; branch on its result: BR_* would compare the floats' bits as integers.
define i32 @forwarded(float %a, float %b) {
; CHECK-LABEL: forwarded:
; CHECK: fle.s
; CHECK: bne
  %s = alloca i32
  %c = fcmp oge float %a, %b
  %z = zext i1 %c to i32
  store i32 %z, ptr %s
  %l = load i32, ptr %s
  %t = icmp ne i32 %l, 0
  br i1 %t, label %x, label %y
x:
  ret i32 1
y:
  ret i32 0
}
define i32 @forwarded_d(double %a, double %b) {
; CHECK-LABEL: forwarded_d:
; CHECK: fle.d
; CHECK: bne
  %s = alloca i32
  %c = fcmp oge double %a, %b
  %z = zext i1 %c to i32
  store i32 %z, ptr %s
  %l = load i32, ptr %s
  %t = icmp ne i32 %l, 0
  br i1 %t, label %x, label %y
x:
  ret i32 1
y:
  ret i32 0
}
