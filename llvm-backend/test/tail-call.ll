; RUN: llc -mtriple=slow32-unknown-none -verify-machineinstrs < %s | FileCheck %s

; A call in tail position is a jump: the frame is popped first, and the
; callee returns to this function's caller.  What may be one, and what
; may not.

%struct.F = type { i32, i32, ptr, ptr, i32, i32 }

declare i32 @slow(ptr)
declare i32 @slow2(ptr, i32, i32)
declare i64 @wide(ptr)
declare i32 @nine(i32, i32, i32, i32, i32, i32, i32, i32, i32)
declare i32 @printf(ptr, ...)
declare void @fill(ptr sret(%struct.F))
declare i32 @takes_byval(ptr byval(%struct.F))
@fp = external global ptr

; The shape this is for: a short path that needs nothing, and "otherwise,
; the general routine".  No call is left in the function, so it has no
; frame: nothing touches the stack pointer or saves the return address.
define i32 @entry(ptr %f) {
; CHECK-LABEL: entry:
; CHECK-NOT: sp
; CHECK-NOT: stw
; CHECK: jalr r0, r31, 0
; CHECK-NOT: sp
; CHECK-NOT: stw
; CHECK: jal r0, slow
  %flag = load i32, ptr %f
  %c = icmp eq i32 %flag, 0
  br i1 %c, label %general, label %short
short:
  ret i32 0
general:
  %r = tail call i32 @slow(ptr %f)
  ret i32 %r
}

; After a real call: what the epilogue restores -- the registers the
; function kept, the return address, the stack pointer -- is restored
; before the jump, and the arguments are in place before that.
define i32 @after_call(ptr %f, i32 %k) {
; CHECK-LABEL: after_call:
; CHECK: stw sp+0, lr
; CHECK: jal r31, slow
; CHECK: add r4, r1, r0
; CHECK: ldw lr, sp+0
; CHECK: addi sp, sp,
; CHECK: jal r0, slow2
  %r = call i32 @slow(ptr %f)
  %t = tail call i32 @slow2(ptr %f, i32 %r, i32 %k)
  ret i32 %t
}

; A 64-bit result comes back in r1:r2 from the callee as it would from here.
define i64 @wide_fwd(ptr %f) {
; CHECK-LABEL: wide_fwd:
; CHECK-NOT: sp
; CHECK: jal r0, wide
  %r = tail call i64 @wide(ptr %f)
  ret i64 %r
}

; A variadic callee whose arguments all fit in registers.
define i32 @to_variadic(ptr %fmt, i32 %a) {
; CHECK-LABEL: to_variadic:
; CHECK: jal r0, printf
  %r = tail call i32 (ptr, ...) @printf(ptr %fmt, i32 %a)
  ret i32 %r
}

; Not in tail position: the result is used.
define i32 @not_tail(ptr %f) {
; CHECK-LABEL: not_tail:
; CHECK: jal r31, slow
; CHECK: addi r1, r1, 1
; CHECK: jalr r0, r31, 0
  %r = call i32 @slow(ptr %f)
  %s = add i32 %r, 1
  ret i32 %s
}

; Arguments on the stack would be written into the frame being popped.
define i32 @stack_args(i32 %a) {
; CHECK-LABEL: stack_args:
; CHECK: jal r31, nine
; CHECK: jalr r0, r31, 0
  %r = tail call i32 @nine(i32 %a, i32 %a, i32 %a, i32 %a, i32 %a, i32 %a, i32 %a, i32 %a, i32 %a)
  ret i32 %r
}

; A byval argument is a copy in this function's frame.
define i32 @byval_arg(ptr %p) {
; CHECK-LABEL: byval_arg:
; CHECK: jal r31, takes_byval
; CHECK: jalr r0, r31, 0
  %r = tail call i32 @takes_byval(ptr byval(%struct.F) %p)
  ret i32 %r
}

; Struct return, on either side.
define void @sret_callee(ptr sret(%struct.F) %out) {
; CHECK-LABEL: sret_callee:
; CHECK: jal r31, fill
; CHECK: jalr r0, r31, 0
  tail call void @fill(ptr sret(%struct.F) %out)
  ret void
}

; ... the callee's alone, too: the pointer is this function's argument,
; not its own struct return.
define void @sret_callee_only(ptr %out) {
; CHECK-LABEL: sret_callee_only:
; CHECK: jal r31, fill
; CHECK: jalr r0, r31, 0
  tail call void @fill(ptr sret(%struct.F) %out)
  ret void
}

; An indirect callee: its address needs a register that neither the
; epilogue restores nor an argument occupies.  r2 is reserved (the
; long-branch scratch), so it is that.
define i32 @indirect(i32 %a) {
; CHECK-LABEL: indirect:
; CHECK-NOT: sp
; CHECK: ldw r2,
; CHECK-NEXT: jalr r0, r2, 0
  %f = load ptr, ptr @fp
  %r = tail call i32 %f(i32 %a)
  ret i32 %r
}

; The pointer is kept in a callee-saved register across a real call; it
; is moved to r2 before the epilogue restores that register.
declare i32 @g(i32)
define i32 @call_then_jump(i32 %a, ptr %f) {
; CHECK-LABEL: call_then_jump:
; CHECK: add [[KEEP:r1[1-9]|r2[0-8]]], r4, r0
; CHECK: jal r31, g
; CHECK: add r2, [[KEEP]], r0
; CHECK: ldw [[KEEP]], sp+
; CHECK: ldw lr, sp+0
; CHECK: addi sp, sp,
; CHECK: jalr r0, r2, 0
  %x = call i32 @g(i32 %a)
  %y = add i32 %x, %a
  %r = tail call i32 %f(i32 %y)
  ret i32 %r
}

; A frame too large for a 12-bit offset: restoring the saved registers
; takes a scratch register, and eight argument registers are live.  The
; scratch is r1 -- not an argument, and not r2.
declare i32 @eight(i32, i32, i32, i32, i32, i32, i32, i32)
@at = external global i32
define i32 @big_frame(i32 %a) {
; CHECK-LABEL: big_frame:
; CHECK: addi sp, sp, -2048
; CHECK: jal r31, g
; CHECK: jal r31, g
; CHECK-NOT: add r{{[3-9]|10}}, sp, r0
; CHECK: ldw lr, sp+0
; CHECK: jal r0, eight
  %arr = alloca [1500 x i32]
  %i = load volatile i32, ptr @at
  %p = getelementptr [1500 x i32], ptr %arr, i32 0, i32 %i
  store i32 %a, ptr %p
  %v = load i32, ptr %p
  %x = call i32 @g(i32 %v)
  %y = call i32 @g(i32 %x)
  %a1 = add i32 %a, 1
  %x1 = add i32 %x, 1
  %y1 = add i32 %y, 1
  %a2 = add i32 %a, 2
  %w = load i32, ptr %p
  %r = tail call i32 @eight(i32 %a, i32 %x, i32 %y, i32 %a1, i32 %x1, i32 %y1, i32 %a2, i32 %w)
  ret i32 %r
}

; A variadic caller is left alone.
define i32 @variadic_caller(ptr %f, ...) {
; CHECK-LABEL: variadic_caller:
; CHECK: jal r31, slow
; CHECK: jalr r0, r31, 0
  %r = tail call i32 @slow(ptr %f)
  ret i32 %r
}

; "disable-tail-calls" is honoured.
define i32 @disabled(ptr %f) "disable-tail-calls"="true" {
; CHECK-LABEL: disabled:
; CHECK: jal r31, slow
; CHECK: jalr r0, r31, 0
  %r = tail call i32 @slow(ptr %f)
  ret i32 %r
}

; musttail, where it can be done.
define i32 @must(ptr %f) {
; CHECK-LABEL: must:
; CHECK-NOT: sp
; CHECK: jal r0, slow
  %r = musttail call i32 @slow(ptr %f)
  ret i32 %r
}
