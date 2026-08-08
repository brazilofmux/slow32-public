; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target
;
; Large memory intrinsics become named libcalls (not hard-wired memcpy for all).

declare void @llvm.memcpy.p0.p0.i32(ptr, ptr, i32, i1)
declare void @llvm.memmove.p0.p0.i32(ptr, ptr, i32, i1)
declare void @llvm.memset.p0.i32(ptr, i8, i32, i1)

define void @copy_big(ptr %d, ptr %s) {
; CHECK-LABEL: copy_big:
; CHECK: jal r31, memcpy
  call void @llvm.memcpy.p0.p0.i32(ptr %d, ptr %s, i32 256, i1 false)
  ret void
}

define void @move_big(ptr %d, ptr %s) {
; CHECK-LABEL: move_big:
; CHECK: jal r31, memmove
  call void @llvm.memmove.p0.p0.i32(ptr %d, ptr %s, i32 256, i1 false)
  ret void
}

define void @set_big(ptr %d) {
; CHECK-LABEL: set_big:
; CHECK: jal r31, memset
  call void @llvm.memset.p0.i32(ptr %d, i8 0, i32 256, i1 false)
  ret void
}
