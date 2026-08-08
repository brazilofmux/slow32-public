; RUN: llc -O0 -mtriple=slow32-unknown-none -verify-machineinstrs < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; A GPRPair (f64/i64) value live across a call is spilled and reloaded as two
; word accesses. These go through the STW_FI/LDW_FI frame-index pseudos, which
; carry a half-offset (0/4) operand that frame-index elimination folds away
; before rewriting them to plain STW/LDW. Previously the spill emitted a STW/LDW
; with an extra explicit operand, which -verify-machineinstrs rejected.

declare void @clobber()

define double @f(double %x) {
; The pseudos must be fully lowered: no STW_FI/LDW_FI may reach the output, and
; the two halves are stored/loaded to adjacent words. Calls force an LR save
; but not necessarily an FP, so addresses are SP-relative.
; CHECK-LABEL: f:
; CHECK-NOT: STW_FI
; CHECK-NOT: LDW_FI
; CHECK: stw sp+0, lr
; CHECK: stw sp+{{[0-9]+}}, r{{[0-9]+}}
; CHECK: stw sp+{{[0-9]+}}, r{{[0-9]+}}
; CHECK: jal r31, clobber
; CHECK: ldw r{{[0-9]+}}, sp+{{[0-9]+}}
; CHECK: ldw r{{[0-9]+}}, sp+{{[0-9]+}}
; CHECK: ldw lr, sp+0
  call void @clobber()
  ret double %x
}
