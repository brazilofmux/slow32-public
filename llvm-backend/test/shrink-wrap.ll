; RUN: llc -O2 -mtriple=slow32-unknown-none -verify-machineinstrs < %s | FileCheck %s
; RUN: llc -O2 -mtriple=slow32-unknown-none -enable-shrink-wrap=false < %s \
; RUN:   | FileCheck %s --check-prefix=NOSW
; REQUIRES: slow32-registered-target

; Shrink-wrapping: the early return needs no frame, so the prologue goes to
; the block that calls (libcob's "short path, else the general routine"
; entries paid for the general routine's frame on every call).

declare void @slow(ptr)

; CHECK-LABEL: early:
; CHECK: # %bb.0:
; CHECK-NOT: sp
; CHECK: beq r4, r{{[0-9]+}}, [[RET:.LBB[0-9_]+]]
; CHECK: addi sp, sp, -
; CHECK: stw sp+0, lr
; CHECK: jal r31, slow
; CHECK: ldw lr, sp+0
; CHECK: addi sp, sp,
; CHECK-NEXT: jalr r0, r31, 0
; CHECK: [[RET]]:
; CHECK-NEXT: addi r1, r0, 0
; CHECK-NEXT: jalr r0, r31, 0
; NOSW-LABEL: early:
; NOSW: # %bb.0:
; NOSW-NEXT: addi sp, sp, -
define i32 @early(ptr %p, i32 %n) nounwind {
entry:
  %z = icmp eq i32 %n, 0
  br i1 %z, label %done, label %work

work:
  call void @slow(ptr %p)
  %v = load i32, ptr %p
  %r = add i32 %v, %n
  ret i32 %r

done:
  ret i32 0
}

; r31 used other than by a call or a return keeps the prologue in the entry
; block: r31 is not in the callee-saved list, so ShrinkWrap cannot see such
; a use, and LR would be saved after it.
; CHECK-LABEL: usesra:
; CHECK-NEXT: # %bb.0:
; CHECK-NEXT: addi sp, sp, -
define ptr @usesra(i32 %n) nounwind {
entry:
  %z = icmp eq i32 %n, 0
  br i1 %z, label %done, label %work

work:
  call void @slow(ptr null)
  br label %done

done:
  %ra = call ptr @llvm.returnaddress(i32 0)
  ret ptr %ra
}
declare ptr @llvm.returnaddress(i32)

; A function that needs unwind information is shrink-wrapped too: the CFA
; moves with the prologue, and the epilogue leaves the state on entry (lr
; restored, the CFA at sp+0) for the frameless return laid out after it.
; CHECK-LABEL: unwinds:
; CHECK: .cfi_startproc
; CHECK-NEXT: # %bb.0:
; CHECK-NOT: sp
; CHECK: beq r4, r{{[0-9]+}}, [[RET:.LBB[0-9_]+]]
; CHECK: addi sp, sp, -16
; CHECK-NEXT: .cfi_def_cfa_offset 16
; CHECK-NEXT: stw sp+0, lr
; CHECK-NEXT: .cfi_offset lr, -16
; CHECK: jal r31, slow
; CHECK: ldw lr, sp+0
; CHECK-NEXT: .cfi_restore lr
; CHECK-NEXT: addi sp, sp, 16
; CHECK-NEXT: .cfi_def_cfa_offset 0
; CHECK-NEXT: jalr r0, r31, 0
; CHECK-NEXT: [[RET]]:
; CHECK-NEXT: addi r1, r0, 0
; CHECK-NEXT: jalr r0, r31, 0
define i32 @unwinds(ptr %p, i32 %n) {
entry:
  %z = icmp eq i32 %n, 0
  br i1 %z, label %done, label %work

work:
  call void @slow(ptr %p)
  ret i32 1

done:
  ret i32 0
}
