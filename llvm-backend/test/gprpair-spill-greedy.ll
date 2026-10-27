; RUN: llc -O2 -mtriple=slow32-unknown-none -verify-machineinstrs < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; Ten doubles live across a call: the greedy allocator spills virtual GPRPairs
; through the inline spiller.  storeRegToStackSlot/loadRegFromStackSlot used
; to emit two sub-register LDW_FI/STW_FI for a virtual pair, each reload half
; an undef partial def; the second undef made the first half dead, so the
; allocator could give the pair a register other than the one the low word
; was loaded into (a wrong-output miscompile, found by cfuzz), and the
; verifier reported "No live segment at def".  A virtual pair is now spilled
; by one LDW64_FI/STW64_FI, split on the physical halves after allocation.

declare void @g()
declare void @use(double, double, double, double, double, double, double, double)

; CHECK-LABEL: f:
; CHECK-NOT: LDW64_FI
; CHECK-NOT: STW64_FI
; CHECK-NOT: LDW_FI
; CHECK-NOT: STW_FI
; CHECK: jal r31, g
; CHECK: jal r31, use
define void @f(ptr %p) {
  %a0 = load double, ptr %p
  %q1 = getelementptr double, ptr %p, i32 1
  %a1 = load double, ptr %q1
  %q2 = getelementptr double, ptr %p, i32 2
  %a2 = load double, ptr %q2
  %q3 = getelementptr double, ptr %p, i32 3
  %a3 = load double, ptr %q3
  %q4 = getelementptr double, ptr %p, i32 4
  %a4 = load double, ptr %q4
  %q5 = getelementptr double, ptr %p, i32 5
  %a5 = load double, ptr %q5
  %q6 = getelementptr double, ptr %p, i32 6
  %a6 = load double, ptr %q6
  %q7 = getelementptr double, ptr %p, i32 7
  %a7 = load double, ptr %q7
  %q8 = getelementptr double, ptr %p, i32 8
  %a8 = load double, ptr %q8
  %q9 = getelementptr double, ptr %p, i32 9
  %a9 = load double, ptr %q9
  call void @g()
  %b0 = fadd double %a0, %a1
  %b1 = fadd double %a2, %a3
  %b2 = fadd double %a4, %a5
  %b3 = fadd double %a6, %a7
  %b4 = fadd double %a8, %a9
  %b5 = fmul double %a0, %a9
  %b6 = fmul double %a1, %a8
  %b7 = fmul double %a2, %a7
  call void @use(double %b0, double %b1, double %b2, double %b3, double %b4, double %b5, double %b6, double %b7)
  ret void
}
