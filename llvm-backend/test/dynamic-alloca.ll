; RUN: llc -mtriple=slow32-unknown-none -O2 < %s | FileCheck %s

; A dynamic alloca moves SP an unknown amount, so the epilogue must first
; recompute SP from FP before reloading the LR/FP save slots at SP+0/+4.
; Previously it reloaded them straight off the moved SP — reading from
; inside the dynamic allocation — and then "restored" a garbage SP.

declare void @use(ptr)

define i32 @dyn(i32 %n) {
; CHECK-LABEL: dyn:
; Prologue: aligned frame, saves, fp = incoming sp.
; CHECK: stw sp+0, lr
; CHECK: stw sp+4, fp
; CHECK: add fp, sp, r0
; CHECK-NEXT: addi fp, fp, 16
; Epilogue: sp is recomputed from fp before the reloads (CFI directives
; may sit between them).
; CHECK: add sp, fp, r0
; CHECK-NEXT: addi sp, sp, -16
; CHECK: ldw lr, sp+0
; CHECK: ldw fp, sp+4
; CHECK: jalr r0, r31, 0
entry:
  %buf = alloca i8, i32 %n
  call void @use(ptr %buf)
  ret i32 0
}
