; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s

; Wide integer (_BitInt) operations must be legalized without reaching for
; 128-bit runtime libcalls the SLOW-32 runtime does not provide: div/rem and
; FP conversions above 64 bits are inline-expanded (MaxDivRemBitWidthSupported
; and MaxLargeFPConvertBitWidthSupported are both 64).

; 64-bit division still uses the runtime's __udivdi3.
define i64 @udiv64(i64 %a, i64 %b) {
; CHECK-LABEL: udiv64:
; CHECK: __udivdi3
; CHECK-NOT: __udivti3
entry:
  %q = udiv i64 %a, %b
  ret i64 %q
}

; 128-bit unsigned division inline-expands: no __udivti3, no __udivei4.
define i128 @udiv128(i128 %a, i128 %b) {
; CHECK-LABEL: udiv128:
; CHECK-NOT: __udivti3
; CHECK-NOT: __udivei4
; CHECK: jalr r0, r31, 0
entry:
  %q = udiv i128 %a, %b
  ret i128 %q
}

; Signed 128-bit remainder likewise stays libcall-free.
define i128 @srem128(i128 %a, i128 %b) {
; CHECK-LABEL: srem128:
; CHECK-NOT: __modti3
; CHECK-NOT: __smodei4
; CHECK: jalr r0, r31, 0
entry:
  %r = srem i128 %a, %b
  ret i128 %r
}

; 128-bit multiply expands to 32-bit mul/mulhu parts, not __multi3.
define i128 @mul128(i128 %a, i128 %b) {
; CHECK-LABEL: mul128:
; CHECK-NOT: __multi3
; CHECK: jalr r0, r31, 0
entry:
  %p = mul i128 %a, %b
  ret i128 %p
}

; A non-power-of-two width exercises the same paths after promotion to i128.
define i66 @udiv66(i66 %a, i66 %b) {
; CHECK-LABEL: udiv66:
; CHECK-NOT: __udivti3
; CHECK: jalr r0, r31, 0
entry:
  %q = udiv i66 %a, %b
  ret i66 %q
}

; u128 -> f64 conversion inline-expands rather than calling __floatuntidf.
define double @u128tof64(i128 %a) {
; CHECK-LABEL: u128tof64:
; CHECK-NOT: __floatuntidf
; CHECK: jalr r0, r31, 0
entry:
  %d = uitofp i128 %a to double
  ret double %d
}

; f64 -> u128 conversion inline-expands rather than calling __fixunsdfti.
define i128 @f64tou128(double %d) {
; CHECK-LABEL: f64tou128:
; CHECK-NOT: __fixunsdfti
; CHECK: jalr r0, r31, 0
entry:
  %v = fptoui double %d to i128
  ret i128 %v
}
