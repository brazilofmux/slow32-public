; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target
;
; i64 division lowers to the standard compiler-rt helpers.

define i64 @sdiv64(i64 %a, i64 %b) {
; CHECK-LABEL: sdiv64:
; CHECK: jal r31, __divdi3
  %q = sdiv i64 %a, %b
  ret i64 %q
}

define i64 @udiv64(i64 %a, i64 %b) {
; CHECK-LABEL: udiv64:
; CHECK: jal r31, __udivdi3
  %q = udiv i64 %a, %b
  ret i64 %q
}

define i32 @udiv32(i32 %a, i32 %b) {
; CHECK-LABEL: udiv32:
; CHECK: jal r31, __udivsi3
  %q = udiv i32 %a, %b
  ret i32 %q
}
