; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target

declare void @bar()

; Call-only: LR save, no FP.
define void @calls_only() {
; CHECK-LABEL: calls_only:
; CHECK: stw sp+0, lr
; CHECK-NOT: stw sp+{{[0-9]+}}, fp
; CHECK: jal r31, bar
; CHECK: ldw lr, sp+0
  call void @bar()
  ret void
}

; Pure leaf: no frame at all.
define i32 @pure_leaf(i32 %a, i32 %b) {
; CHECK-LABEL: pure_leaf:
; CHECK-NOT: addi sp,
; CHECK: add r1, r3, r4
; CHECK: jalr r0, r31, 0
  %s = add i32 %a, %b
  ret i32 %s
}

; Frame address taken forces FP.
define ptr @needs_fp() {
; CHECK-LABEL: needs_fp:
; CHECK: stw {{.*}}, fp
; CHECK: addi fp, {{.*}}
  %p = call ptr @llvm.frameaddress.p0(i32 0)
  ret ptr %p
}
declare ptr @llvm.frameaddress.p0(i32)
