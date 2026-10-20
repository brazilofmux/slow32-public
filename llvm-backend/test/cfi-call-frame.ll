; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target
;
; A call that passes arguments on the stack moves SP around itself
; (hasReservedCallFrame is false).  Without a frame pointer the CFA is
; SP-relative, so each move needs .cfi_adjust_cfa_offset: without them an
; unwinder at the call read the saved LR from the outgoing arguments, and an
; exception thrown by the callee was lost (libc++'s __throw_system_error).
; With a frame pointer the CFA is FP-relative and must not be adjusted.

declare void @ten(i32, i32, i32, i32, i32, i32, i32, i32, i32, i32)

define void @frameless_stack_args(i32 %a) {
; CHECK-LABEL: frameless_stack_args:
; CHECK: .cfi_def_cfa_offset 16
; CHECK: addi sp, sp, -16
; CHECK-NEXT: .cfi_adjust_cfa_offset 16
; CHECK: jal r31, ten
; CHECK-NEXT: addi sp, sp, 16
; CHECK-NEXT: .cfi_adjust_cfa_offset -16
; CHECK: .cfi_endproc
  call void @ten(i32 %a, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7, i32 8, i32 9)
  ret void
}

define void @with_fp_stack_args(i32 %n) {
; CHECK-LABEL: with_fp_stack_args:
; CHECK: .cfi_def_cfa fp, 0
; CHECK-NOT: .cfi_adjust_cfa_offset
; CHECK: .cfi_endproc
  %buf = alloca i8, i32 %n
  call void @ten(i32 %n, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7, i32 8, i32 9)
  ret void
}
