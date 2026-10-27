; RUN: llc -O2 -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; SELECT is lowered branchless, F ^ ((T ^ F) & -C), which computes both arms
; whatever C is.  The arm the select discards may be poison, so it is frozen
; first.  Without the freeze, "sub nuw 4, y" (nuw: y <= 4, and y is odd)
; let the combiner fold T ^ F = 4 ^ (4 - y) into 8 - y, wrong for y = 7,
; where the select takes 4.  CorrelatedValuePropagation makes this shape from
; "4 % y"; cfuzz found it.

; CHECK-LABEL: urem4:
; CHECK-NOT: addi r{{[0-9]+}}, r0, 8
; CHECK: xori r{{[0-9]+}}, r{{[0-9]+}}, 4
; CHECK: jalr r0, r31, 0
define i32 @urem4(i32 %x) {
  %y = or i32 %x, 1
  %c = icmp ult i32 4, %y
  %s = sub nuw i32 4, %y
  %r = select i1 %c, i32 4, i32 %s
  ret i32 %r
}
