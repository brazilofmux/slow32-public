; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; Variable 64-bit shifts must stay correct when the runtime shift amount is 0.
; The hardware masks shift amounts to 5 bits, so the high/low cross term must be
; computed as (x >> 1) >> (Shamt ^ 31) rather than x >> (32 - Shamt), which would
; degenerate to x >> 0 for Shamt == 0 and corrupt the result.

define i64 @shl_var(i64 %x, i64 %n) {
; CHECK-LABEL: shl_var:
; CHECK-NOT: sub r{{[0-9]+}}, r{{[0-9]+}}, r{{[0-9]+}}
; CHECK: xori r{{[0-9]+}}, r{{[0-9]+}}, 31
; CHECK: srli r{{[0-9]+}}, r{{[0-9]+}}, 1
  %r = shl i64 %x, %n
  ret i64 %r
}

define i64 @srl_var(i64 %x, i64 %n) {
; CHECK-LABEL: srl_var:
; CHECK: xori r{{[0-9]+}}, r{{[0-9]+}}, 31
; CHECK: slli r{{[0-9]+}}, r{{[0-9]+}}, 1
  %r = lshr i64 %x, %n
  ret i64 %r
}

define i64 @sra_var(i64 %x, i64 %n) {
; CHECK-LABEL: sra_var:
; CHECK: xori r{{[0-9]+}}, r{{[0-9]+}}, 31
; CHECK: slli r{{[0-9]+}}, r{{[0-9]+}}, 1
  %r = ashr i64 %x, %n
  ret i64 %r
}
