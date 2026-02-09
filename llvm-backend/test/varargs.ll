; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s

; Make sure we spill unused argument registers r4-r10 to the varargs area.
; The caller packs args sequentially (no pair alignment gaps) and the callee
; saves all remaining arg registers into a contiguous save area that va_arg
; walks linearly.

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
; Save area at fp-28, storing r4-r10 (7 registers, 28 bytes)
; CHECK: addi [[BASE:r[0-9]+]], fp, -28
; Stores to the save area (order may vary)
; CHECK-DAG: stw {{.*}}, r10
; CHECK-DAG: stw {{.*}}, r9
; CHECK-DAG: stw {{.*}}, r8
; CHECK-DAG: stw {{.*}}, r7
; CHECK-DAG: stw {{.*}}, r6
; CHECK-DAG: stw {{.*}}, r5
; CHECK-DAG: stw [[BASE]]+0, r4

; Regression test: calling a varargs function with 4 doubles.
; With sequential packing in R3-R10: fmt in r3, doubles 1-3 in r4:r5,
; r6:r7, r8:r9.  Double 4 splits: lo in r10, hi on stack.
declare i32 @varargs_f64(ptr, ...)

define i32 @call_with_4_doubles(ptr %fmt) {
entry:
  %ret = call i32 (ptr, ...) @varargs_f64(ptr %fmt, double 1.0, double 2.0, double 3.0, double 4.0)
  ret i32 %ret
}

; CHECK-LABEL: call_with_4_doubles:
; Double 4 hi half loaded and stored to stack; lo half goes in r10
; CHECK: ldw [[D4HI:r[0-9]+]], {{.*}}+4
; CHECK: ldw r10, {{.*}}+0
; CHECK: stw sp+0, [[D4HI]]
; Doubles 1-3 in register pairs r4:r5, r6:r7, r8:r9
; CHECK-DAG: ldw r4,
; CHECK-DAG: ldw r5,
; CHECK-DAG: ldw r6,
; CHECK-DAG: ldw r7,
; CHECK-DAG: ldw r8,
; CHECK-DAG: ldw r9,
; CHECK: jal r31, varargs_f64
