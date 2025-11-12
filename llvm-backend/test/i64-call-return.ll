; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s

; Basic callee: ensure the i64 return value stays entirely in r1/r2 with no
; local spill slot materialised before the epilogue.
define i64 @add64(i64 %x, i64 %y) {
; CHECK-LABEL: add64:
; CHECK: add r4, r4, r6
; CHECK: add r1, r3, r5
; CHECK: sltu r3, r1, r3
; CHECK: add r2, r4, r3
; CHECK-NOT: stw
; CHECK: jalr r0, r31, 0
entry:
  %sum = add i64 %x, %y
  ret i64 %sum
}

; Call a helper with wide immediates and make sure the arguments flow through
; the r3/r4 and r5/r6 register pairs directly.
define i64 @use_add64() {
; CHECK-LABEL: use_add64:
; CHECK: addi r3
; CHECK: addi r4
; CHECK: addi r5
; CHECK: addi r6
; CHECK-NOT: stw
; CHECK: jal r31, add64
; CHECK: jalr r0, r31, 0
entry:
  %call = call i64 @add64(i64 4822678189205117032, i64 617283945678901234)
  ret i64 %call
}

; Returning a constant should materialise the value directly in r1/r2.
define i64 @ret_const64() {
; CHECK-LABEL: ret_const64:
; CHECK: lui r1
; CHECK: addi r1
; CHECK: lui r3
; CHECK: addi r2, r3
; CHECK-NOT: stw
; CHECK: jalr r0, r31, 0
entry:
  ret i64 81985529216486895
}
