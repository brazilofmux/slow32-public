; RUN: llc -mtriple=slow32-unknown-none -O2 < %s | FileCheck %s --check-prefix=SPEED
; RUN: llc -mtriple=slow32-unknown-none -O2 < %s | FileCheck %s --check-prefix=SIZE
; REQUIRES: slow32-registered-target
;
; Default lowering uses the branchless mask form. Optsize uses SELECT_PSEUDO
; (branch + phi).

define i32 @sel_speed(i32 %c, i32 %a, i32 %b) {
; SPEED-LABEL: sel_speed:
; SPEED: sub
; SPEED: and
; SPEED: xor
  %cmp = icmp ne i32 %c, 0
  %r = select i1 %cmp, i32 %a, i32 %b
  ret i32 %r
}

define i32 @sel_size(i32 %c, i32 %a, i32 %b) #0 {
; SIZE-LABEL: sel_size:
; SIZE: bne
  %cmp = icmp ne i32 %c, 0
  %r = select i1 %cmp, i32 %a, i32 %b
  ret i32 %r
}

attributes #0 = { optsize }
