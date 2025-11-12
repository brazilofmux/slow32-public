; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s
; REQUIRES: slow32-registered-target

@arr = global [1024 x i32] zeroinitializer, align 4
@glob = global i32 0, align 4

; Verify that a large GEP on a global folds the offset into the %hi/%lo pair
; instead of synthesising the displacement in a temporary register.
define i32 @load_gep_large() {
; CHECK-LABEL: load_gep_large:
; CHECK: lui [[BASE:r[0-9]+]], %hi(arr+2048)
; CHECK-NEXT: addi [[BASE]], [[BASE]], %lo(arr+2048)
; CHECK-NEXT: ldw [[VAL:r[0-9]+]], [[BASE]]+0
; CHECK-NEXT: jalr r0, r31, 0
; CHECK-NOT: lui r{{[0-9]+}}, 1
; CHECK-NOT: add r{{[0-9]+}}, r{{[0-9]+}}, r{{[0-9]+}}
entry:
  %gep = getelementptr inbounds [1024 x i32], ptr @arr, i32 0, i32 512
  %val = load i32, ptr %gep
  ret i32 %val
}

; Returning a pointer to a global + offset should use the same folding.
define ptr @return_ptr_gep_large() {
; CHECK-LABEL: return_ptr_gep_large:
; CHECK: lui [[BASE:r[0-9]+]], %hi(arr+2048)
; CHECK-NEXT: addi [[BASE]], [[BASE]], %lo(arr+2048)
; CHECK-NEXT: jalr r0, r31, 0
; CHECK-NOT: add r{{[0-9]+}}, r{{[0-9]+}}, r{{[0-9]+}}
entry:
  %gep = getelementptr inbounds [1024 x i32], ptr @arr, i32 0, i32 512
  ret ptr %gep
}

; Small offsets should still use the base+immediate form.
define i32 @load_gep_small(ptr %p) {
; CHECK-LABEL: load_gep_small:
; CHECK: ldw r{{[0-9]+}}, r{{[0-9]+}}+28
entry:
  %gep = getelementptr inbounds i32, ptr %p, i32 7
  %val = load i32, ptr %gep
  ret i32 %val
}

; Folding also applies when starting from an existing LOAD_ADDR.
define i32 @reload_from_global() {
; CHECK-LABEL: reload_from_global:
; CHECK: lui [[BASE:r[0-9]+]], %hi(glob)
; CHECK-NEXT: addi [[BASE]], [[BASE]], %lo(glob)
; CHECK-NEXT: ldw [[VAL:r[0-9]+]], [[BASE]]+4
; CHECK-NEXT: jalr r0, r31, 0
entry:
  %ptr = getelementptr inbounds i32, ptr @glob, i32 1
  %val = load i32, ptr %ptr
  ret i32 %val
}
