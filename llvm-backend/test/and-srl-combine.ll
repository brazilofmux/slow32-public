; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; (and (srl x, 16), 0xFFFF) -> (srl x, 16) is only valid for i32. For i64 the
; mask still clears bits 16-63, so the high word must remain zero.

define i64 @and_srl_i64(i64 %x) {
; CHECK-LABEL: and_srl_i64:
; CHECK: srli r1, r3, 16
; CHECK: addi r2, r0, 0
  %s = lshr i64 %x, 16
  %a = and i64 %s, 65535
  ret i64 %a
}

; The i32 fold is still applied: a bare srli with no redundant andi.
define i32 @and_srl_i32(i32 %x) {
; CHECK-LABEL: and_srl_i32:
; CHECK: srli r1, r3, 16
; CHECK-NOT: andi
  %s = lshr i32 %x, 16
  %a = and i32 %s, 65535
  ret i32 %a
}
