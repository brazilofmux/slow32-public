; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; A fixed f64 parameter of a varargs function that straddles the register/stack
; boundary (seven leading i32s fill R3-R9, leaving only R10) is passed as
; lo->R10, hi->[sp+0]. The callee must read it the same way. Previously the
; callee assigned the whole f64 to the stack and read garbage.

declare i32 @va(i32, i32, i32, i32, i32, i32, i32, double, ...)

; CHECK-LABEL: caller:
; LR saved, then real call-frame adjust so stack args sit below LR.
; CHECK: stw sp+0, lr
; CHECK: addi sp, sp, -8
; Low half of the double goes in R10, high half is stored at the top of the
; outgoing argument area.
; CHECK-DAG: ldw r10,
; CHECK-DAG: stw sp+0,
; CHECK: jal r31, va
; CHECK: addi sp, sp, 8
; CHECK: ldw lr, sp+0
define i32 @caller() {
  %r = call i32 (i32, i32, i32, i32, i32, i32, i32, double, ...) @va(
        i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7, double 3.5, i32 99)
  ret i32 %r
}

; CHECK-LABEL: callee:
; The returned double's low half comes from R10 and its high half from the
; incoming stack slot at sp+0 (leaf, no frame).
; CHECK: add r1, r10, r0
; CHECK: ldw r2, sp+0
define double @callee(i32 %a1, i32 %a2, i32 %a3, i32 %a4, i32 %a5, i32 %a6, i32 %a7, double %d, ...) {
  ret double %d
}
