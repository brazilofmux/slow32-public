; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; A structure passed through the ellipsis travels by reference: the
; address of the caller's copy goes in the next argument slot, where
; va_arg (clang lowers va_arg of an aggregate as a pointer load) looks for
; it.  It used to be given stack space of its own, out of the slot order,
; and the callee read the following argument as the structure's address.
; A fixed structure parameter keeps the byval convention.

%struct.T = type { i32, i32, i32 }
declare i32 @v(i32, ...)
declare void @f(i32, ptr byval(%struct.T) align 4, i32)

; CHECK-LABEL: caller:
; CHECK: addi [[COPY:r[0-9]+]], sp, 16
; CHECK: jal r31, memcpy
; CHECK-DAG: addi r3, r0, 1
; CHECK-DAG: addi r5, r0, 42
; CHECK-DAG: add r4, [[COPY]], r0
; CHECK: jal r31, v
define i32 @caller(ptr %p) {
  %r = call i32 (i32, ...) @v(i32 1, ptr byval(%struct.T) align 4 %p, i32 42)
  ret i32 %r
}

; CHECK-LABEL: fixed:
; CHECK: jal r31, memcpy
; CHECK: stw sp+0, [[COPY2:r[0-9]+]]
; CHECK-DAG: addi r3, r0, 1
; CHECK-DAG: addi r4, r0, 42
; CHECK: jal r31, f
define void @fixed(ptr %p) {
  call void @f(i32 1, ptr byval(%struct.T) align 4 %p, i32 42)
  ret void
}
