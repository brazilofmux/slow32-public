; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s

; Make sure we spill any unused argument registers to the varargs area and
; that the va_list pointer is initialised with the start of that spill slot.

declare void @llvm.va_start.p0(ptr)

define i32 @consume(i32 %count, ...) {
entry:
  %ap = alloca ptr, align 4
  call void @llvm.va_start.p0(ptr %ap)
  %apv = load ptr, ptr %ap, align 4
  %p = bitcast ptr %apv to ptr
  %v0 = load i32, ptr %p, align 4
  %inc = getelementptr inbounds i32, ptr %p, i32 1
  %v1 = load i32, ptr %inc, align 4
  %sum = add i32 %v0, %v1
  %res = add i32 %sum, %count
  ret i32 %res
}

; CHECK-LABEL: consume:
; CHECK: addi fp, fp, 40
; CHECK: addi [[BASE:r[0-9]+]], fp, -28
; CHECK: stw [[BASE]]+24, r10
; CHECK: stw [[BASE]]+20, r9
; CHECK: stw [[BASE]]+16, r8
; CHECK: stw [[BASE]]+12, r7
; CHECK: stw [[BASE]]+8, r6
; CHECK: stw [[BASE]]+4, r5
; CHECK: stw [[BASE]]+0, r4
