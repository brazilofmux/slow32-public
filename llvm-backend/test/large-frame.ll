; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target
;
; Verify that large stack frames and deep frame-index offsets expand into
; legal 12-bit immediates: staged ADDI chunks for the SP adjustments, and
; LUI+ADD materialisation for large frame-index offsets.

declare void @sink(ptr)

define i32 @large(i32 %v) {
; CHECK-LABEL: large:
; Prologue drops the 12032-byte frame (12000 locals + LR save, rounded to
; the 16-byte stack alignment) in legal chunks.
; CHECK: addi sp, sp, -2048
; CHECK-NEXT: addi sp, sp, -2048
; CHECK-NEXT: addi sp, sp, -2048
; CHECK-NEXT: addi sp, sp, -2048
; CHECK-NEXT: addi sp, sp, -2048
; CHECK-NEXT: addi sp, sp, -1792
; LR only (no FP required for a plain alloca + call); CFI directives may
; sit between the drop and the save.
; CHECK: stw sp+0, lr
;
entry:
  %buf = alloca [3000 x i32], align 4
  %ptr = getelementptr inbounds [3000 x i32], ptr %buf, i32 0, i32 2500
  store i32 %v, ptr %ptr, align 4
; &buf[2500] = sp + 28 + 10000: the out-of-range 10000 is materialised with
; LUI+ADDI (0x2000 + 1808) and added to the in-range sp+28 base.
; CHECK: lui [[BIG:r[0-9]+]], 2
; CHECK-NEXT: addi [[BIG]], [[BIG]], 1808
; CHECK-NEXT: addi [[BASE:r[0-9]+]], sp, 28
; CHECK-NEXT: add [[ADDR:r[0-9]+]], [[BASE]], [[BIG]]
; CHECK: stw [[ADDR]]+0,
  %val = load i32, ptr %ptr, align 4
  call void @sink(ptr %ptr)
  ret i32 %val

; Epilogue restores LR and splits the stack restore into legal pieces.
; CHECK: ldw lr, sp+0
; CHECK: addi sp, sp, 2047
; CHECK-NEXT: addi sp, sp, 2047
; CHECK-NEXT: addi sp, sp, 2047
; CHECK-NEXT: addi sp, sp, 2047
; CHECK-NEXT: addi sp, sp, 2047
; CHECK-NEXT: addi sp, sp, 1797
; CHECK: jalr r0, r31, 0
}
