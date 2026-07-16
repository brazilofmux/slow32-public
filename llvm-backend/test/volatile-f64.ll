; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; f64 loads/stores are split into two i32 accesses. The split halves must carry
; the original volatile flag, otherwise repeated volatile accesses get CSE'd or
; eliminated. Three volatile loads and three volatile stores of an f64 must each
; produce three pairs of word accesses (six ldw, six stw), not one.

define void @f(ptr %p, ptr %q) {
; CHECK-LABEL: f:
; CHECK-COUNT-6: ldw r{{[0-9]+}},
; CHECK-NOT: ldw r{{[0-9]+}},
; CHECK-COUNT-6: stw
  %a = load volatile double, ptr %p
  %b = load volatile double, ptr %p
  %c = load volatile double, ptr %p
  store volatile double %a, ptr %q
  store volatile double %b, ptr %q
  store volatile double %c, ptr %q
  ret void
}
