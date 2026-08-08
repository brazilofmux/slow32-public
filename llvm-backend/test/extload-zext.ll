; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target
;
; Any-extending byte/half loads must zero-extend (LDBU/LDHU), not sign-extend.
; High bits of the result must be zero when the loaded value is used as i32.

define i32 @load_i8_anyext(ptr %p) {
; CHECK-LABEL: load_i8_anyext:
; CHECK: ldbu {{r[0-9]+}}, {{r[0-9]+}}+0
  %v = load i8, ptr %p, align 1
  %e = zext i8 %v to i32
  ret i32 %e
}

define i32 @load_i16_anyext(ptr %p) {
; CHECK-LABEL: load_i16_anyext:
; CHECK: ldhu {{r[0-9]+}}, {{r[0-9]+}}+0
  %v = load i16, ptr %p, align 2
  %e = zext i16 %v to i32
  ret i32 %e
}

; Truncating add forces an anyext-style load path through i8 memory.
define i32 @load_i8_add(ptr %p, i32 %x) {
; CHECK-LABEL: load_i8_add:
; CHECK: ldbu
  %v = load i8, ptr %p, align 1
  %e = zext i8 %v to i32
  %s = add i32 %e, %x
  ret i32 %s
}
