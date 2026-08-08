; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target
;
; Smoke-check that functions with calls emit basic DWARF CFI for LR.

declare void @ext()

define void @with_call() {
; CHECK-LABEL: with_call:
; CHECK: .cfi_startproc
; CHECK: addi sp, sp,
; CHECK: .cfi_def_cfa_offset
; CHECK: stw {{.*}}, lr
; CHECK: .cfi_offset lr,
; CHECK: jal r31, ext
; CHECK: ldw lr,
; CHECK: .cfi_restore lr
; CHECK: .cfi_endproc
  call void @ext()
  ret void
}
