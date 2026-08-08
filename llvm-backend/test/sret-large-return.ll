; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; A return value that does not fit in the return registers (R1, plus R2 for the
; high half) must be demoted to an sret pointer. Previously RetCC_SLOW32 fell
; through to a stack assignment that neither caller nor callee agreed on, and
; an i128 return crashed the scheduler. CanLowerReturn now forces indirect
; return so this compiles and round-trips through memory.

define i128 @ret_i128(i128 %a, i128 %b) {
; CHECK-LABEL: ret_i128:
; The result is written through the hidden sret pointer, not returned in regs.
; CHECK: stw
; CHECK: jalr r0, r31, 0
  %r = add i128 %a, %b
  ret i128 %r
}

define void @caller(i128 %a, i128 %b, ptr %out) {
; CHECK-LABEL: caller:
; The return slot address is passed to the callee in the first argument reg R3
; (materialised as add/addi from a stack temp or register).
; CHECK: {{add|addi}} r3,
; CHECK: jal r31, ret_i128
  %r = call i128 @ret_i128(i128 %a, i128 %b)
  store i128 %r, ptr %out
  ret void
}
