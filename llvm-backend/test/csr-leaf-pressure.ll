; RUN: llc -mtriple=slow32-unknown-none -O2 < %s | FileCheck %s

; A leaf function under enough register pressure that the allocator hands
; out r30/r31 as scratch. Both are allocatable in leaves by design, so the
; prologue must save and the epilogue must restore whichever ones were
; actually used — r31 still holds the return address at the jalr, and r30
; is call-preserved from the caller's point of view. Previously neither
; was saved: the function loaded data into lr and returned through it.

define i32 @leaf_pressure(ptr %p, i32 %x) {
; CHECK-LABEL: leaf_pressure:
; CHECK: stw sp+0, lr
; CHECK: stw sp+4, fp
; CHECK: ldw lr, sp+0
; CHECK: ldw fp, sp+4
; CHECK: jalr r0, r31, 0
entry:
  %v0 = load i32, ptr %p
  %g1 = getelementptr i32, ptr %p, i32 1
  %v1 = load i32, ptr %g1
  %g2 = getelementptr i32, ptr %p, i32 2
  %v2 = load i32, ptr %g2
  %g3 = getelementptr i32, ptr %p, i32 3
  %v3 = load i32, ptr %g3
  %g4 = getelementptr i32, ptr %p, i32 4
  %v4 = load i32, ptr %g4
  %g5 = getelementptr i32, ptr %p, i32 5
  %v5 = load i32, ptr %g5
  %g6 = getelementptr i32, ptr %p, i32 6
  %v6 = load i32, ptr %g6
  %g7 = getelementptr i32, ptr %p, i32 7
  %v7 = load i32, ptr %g7
  %g8 = getelementptr i32, ptr %p, i32 8
  %v8 = load i32, ptr %g8
  %g9 = getelementptr i32, ptr %p, i32 9
  %v9 = load i32, ptr %g9
  %g10 = getelementptr i32, ptr %p, i32 10
  %v10 = load i32, ptr %g10
  %g11 = getelementptr i32, ptr %p, i32 11
  %v11 = load i32, ptr %g11
  %g12 = getelementptr i32, ptr %p, i32 12
  %v12 = load i32, ptr %g12
  %g13 = getelementptr i32, ptr %p, i32 13
  %v13 = load i32, ptr %g13
  %g14 = getelementptr i32, ptr %p, i32 14
  %v14 = load i32, ptr %g14
  %g15 = getelementptr i32, ptr %p, i32 15
  %v15 = load i32, ptr %g15
  %g16 = getelementptr i32, ptr %p, i32 16
  %v16 = load i32, ptr %g16
  %g17 = getelementptr i32, ptr %p, i32 17
  %v17 = load i32, ptr %g17
  %g18 = getelementptr i32, ptr %p, i32 18
  %v18 = load i32, ptr %g18
  %g19 = getelementptr i32, ptr %p, i32 19
  %v19 = load i32, ptr %g19
  %g20 = getelementptr i32, ptr %p, i32 20
  %v20 = load i32, ptr %g20
  %g21 = getelementptr i32, ptr %p, i32 21
  %v21 = load i32, ptr %g21
  %g22 = getelementptr i32, ptr %p, i32 22
  %v22 = load i32, ptr %g22
  %g23 = getelementptr i32, ptr %p, i32 23
  %v23 = load i32, ptr %g23
  %g24 = getelementptr i32, ptr %p, i32 24
  %v24 = load i32, ptr %g24
  %g25 = getelementptr i32, ptr %p, i32 25
  %v25 = load i32, ptr %g25
  %g26 = getelementptr i32, ptr %p, i32 26
  %v26 = load i32, ptr %g26
  %g27 = getelementptr i32, ptr %p, i32 27
  %v27 = load i32, ptr %g27
  %g28 = getelementptr i32, ptr %p, i32 28
  %v28 = load i32, ptr %g28
  %g29 = getelementptr i32, ptr %p, i32 29
  %v29 = load i32, ptr %g29
  %m0 = mul i32 %v0, %v29
  %m1 = mul i32 %v1, %v28
  %m2 = mul i32 %v2, %v27
  %m3 = mul i32 %v3, %v26
  %m4 = mul i32 %v4, %v25
  %m5 = mul i32 %v5, %v24
  %m6 = mul i32 %v6, %v23
  %m7 = mul i32 %v7, %v22
  %m8 = mul i32 %v8, %v21
  %m9 = mul i32 %v9, %v20
  %m10 = mul i32 %v10, %v19
  %m11 = mul i32 %v11, %v18
  %m12 = mul i32 %v12, %v17
  %m13 = mul i32 %v13, %v16
  %m14 = mul i32 %v14, %v15
  %s1 = add i32 %m0, %m1
  %s2 = add i32 %s1, %m2
  %s3 = add i32 %s2, %m3
  %s4 = add i32 %s3, %m4
  %s5 = add i32 %s4, %m5
  %s6 = add i32 %s5, %m6
  %s7 = add i32 %s6, %m7
  %s8 = add i32 %s7, %m8
  %s9 = add i32 %s8, %m9
  %s10 = add i32 %s9, %m10
  %s11 = add i32 %s10, %m11
  %s12 = add i32 %s11, %m12
  %s13 = add i32 %s12, %m13
  %s14 = add i32 %s13, %m14
  %r = add i32 %s14, %x
  ret i32 %r
}

; A small leaf that touches neither register keeps its zero-cost frame.
define i32 @small_leaf(i32 %a, i32 %b) {
; CHECK-LABEL: small_leaf:
; CHECK-NOT: stw
; CHECK-NOT: addi sp
; CHECK: jalr r0, r31, 0
entry:
  %r = add i32 %a, %b
  ret i32 %r
}
