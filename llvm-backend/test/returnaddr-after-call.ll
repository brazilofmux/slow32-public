; RUN: llc -O2 -mtriple=slow32-unknown-none -verify-machineinstrs < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; llvm.returnaddress(0) is r31 as it was on entry.  It was a copy from the
; physical r31 where the intrinsic was used, which after a call is the
; address the call returned to -- inside this function, not its caller.
; The machine verifier rejected it ("Using an undefined physical register").

declare void @g()

; CHECK-LABEL: ra_after_call:
; CHECK: add [[SAVED:r[0-9]+]], lr, r0
; CHECK: jal r31, g
; CHECK: add r1, [[SAVED]], r0
; CHECK: jalr r0, r31, 0
define ptr @ra_after_call() nounwind {
  call void @g()
  %ra = call ptr @llvm.returnaddress(i32 0)
  ret ptr %ra
}
declare ptr @llvm.returnaddress(i32)
