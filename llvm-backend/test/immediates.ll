; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target

@glob = global i32 0

; Simple positive constant
define i32 @const_basic() {
; CHECK-LABEL: const_basic:
; CHECK: addi r{{[0-9]+}}, r0, 123
; CHECK-NOT: lui
  ret i32 123
}

; Constant that keeps low bits intact
define i32 @const_hi_lo() {
; CHECK-LABEL: const_hi_lo:
; CHECK: lui r{{[0-9]+}}, 74565
; CHECK: addi r{{[0-9]+}}, r{{[0-9]+}}, 1656
  ret i32 305419896
}

; Constant where rounding bumps the high bits and emits a negative low part
define i32 @const_round_up() {
; CHECK-LABEL: const_round_up:
; CHECK: lui r{{[0-9]+}}, 74566
; CHECK: addi r{{[0-9]+}}, r{{[0-9]+}}, -1
  ret i32 305422335
}

; A load from a global: %hi in the LUI, %lo in the load's offset
define i32 @load_global() {
; CHECK-LABEL: load_global:
; CHECK: lui [[R:r[0-9]+]], %hi(glob)
; CHECK-NEXT: ldw r{{[0-9]+}}, [[R]]+%lo(glob)
  %v = load i32, ptr @glob
  ret i32 %v
}
