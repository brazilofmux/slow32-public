; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
;
; Ensure that the mulhu instruction is used for unsigned 64-bit multiplication,
; resulting in a compact 6-instruction sequence.

define i64 @mul64(i64 %a, i64 %b) nounwind {
; CHECK-LABEL: mul64:
; CHECK:       # %bb.0:
; CHECK-NEXT:    mul [[T1:r[0-9]+]], r3, r6
; CHECK-NEXT:    mulhu [[H1:r[0-9]+]], r3, r5
; CHECK-NEXT:    add [[T1]], [[H1]], [[T1]]
; CHECK-NEXT:    mul [[T2:r[0-9]+]], r4, r5
; CHECK-NEXT:    add r2, [[T1]], [[T2]]
; CHECK-NEXT:    mul r1, r3, r5
; CHECK-NEXT:    jalr r0, r31, 0
entry:
  %p = mul i64 %a, %b
  ret i64 %p
}
