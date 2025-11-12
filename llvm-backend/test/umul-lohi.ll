; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
;
; Ensure that the custom UMUL_LOHI expansion keeps every 16-bit partial
; product separate and feeds the carries into the high word.  The pattern below
; captures the key masks, shifts, and adds so regressions in the multiply-high
; logic trigger a meaningful failure.

define i64 @mul64(i64 %a, i64 %b) nounwind {
; CHECK-LABEL: mul64:
; CHECK: lui [[MASKHI:[^,]+]], 16
; CHECK: addi [[MASK:[^,]+]], [[MASKHI]], -1
; CHECK: and [[BL:[^,]+]], {{[^,]+}}, [[MASK]]
; CHECK: srli [[AH:[^,]+]], {{[^,]+}}, 16
; CHECK: mul [[P2:[^,]+]], [[AH]], [[BL]]
; CHECK: and [[P2LO:[^,]+]], [[P2]], [[MASK]]
; CHECK: and [[AL:[^,]+]], {{[^,]+}}, [[MASK]]
; CHECK: srli [[BH:[^,]+]], {{[^,]+}}, 16
; CHECK: mul [[P1:[^,]+]], [[AL]], [[BH]]
; CHECK: and [[P1LO:[^,]+]], [[P1]], [[MASK]]
; CHECK: add [[MID:[^,]+]], [[P1LO]], [[P2LO]]
; CHECK: mul [[P0:[^,]+]], [[AL]], [[BL]]
; CHECK: srli [[P0HI:[^,]+]], [[P0]], 16
; CHECK: add [[MID]], [[MID]], [[P0HI]]
; CHECK: slli [[SHIFT:[^,]+]], [[MID]], 16
; CHECK: and [[P0LO:[^,]+]], [[P0]], [[MASK]]
; CHECK: or {{[^,]+}}, [[P0LO]], [[SHIFT]]
; CHECK: srli [[MIDHI:[^,]+]], [[MID]], 16
; CHECK: mul [[P3:[^,]+]], [[AH]], [[BH]]
; CHECK: srli [[P1HI:[^,]+]], [[P1]], 16
; CHECK: add [[HIACC:[^,]+]], [[P3]], [[P1HI]]
; CHECK: srli [[P2HI:[^,]+]], [[P2]], 16
; CHECK: add [[HIACC]], [[HIACC]], [[P2HI]]
; CHECK: add {{[^,]+}}, [[HIACC]], [[MIDHI]]
entry:
  %p = mul i64 %a, %b
  ret i64 %p
}
