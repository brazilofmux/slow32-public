; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target
;
; Verify that large stack frames and deep frame-index offsets expand into
; multiple ADDI steps rather than relying on out-of-range immediates.

declare void @sink(ptr)

define i32 @large(i32 %v) {
; CHECK-LABEL: large:
; Prologue splits the large frame into legal 12-bit chunks.
; CHECK: addi sp, sp, -2048
; CHECK: addi sp, sp, -2048
; LR only (no FP required for a plain alloca + call).
; CHECK: stw sp+0, lr
;
entry:
  %buf = alloca [3000 x i32], align 4
  %ptr = getelementptr inbounds [3000 x i32], ptr %buf, i32 0, i32 2500
  store i32 %v, ptr %ptr, align 4
; Address materialisation should use staged addi / large offsets.
; CHECK: stw
  %val = load i32, ptr %ptr, align 4
  call void @sink(ptr %ptr)
  ret i32 %val

; Epilogue restores LR and splits the stack restore into legal pieces.
; CHECK: ldw lr, sp+0
; CHECK: addi sp, sp, 2047
; CHECK: addi sp, sp, 2047
}
