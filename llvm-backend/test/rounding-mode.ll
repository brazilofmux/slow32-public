; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; docs/SPEC.md has one rounding mode: reading it is a constant, setting it nothing.
define i32 @get() {
; CHECK-LABEL: get:
; CHECK: addi r1, r0, 1
  %r = call i32 @llvm.get.rounding()
  ret i32 %r
}
define void @set() {
; CHECK-LABEL: set:
; CHECK-NOT: jal
; CHECK: jalr r0, r31, 0
  call void @llvm.set.rounding(i32 0)
  ret void
}
declare i32 @llvm.get.rounding()
declare void @llvm.set.rounding(i32)
