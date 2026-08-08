; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s --check-prefix=WITHM
; RUN: llc -mtriple=slow32-unknown-none -mattr=-m < %s | FileCheck %s --check-prefix=NOM
; REQUIRES: slow32-registered-target
;
; With +m (default) integer multiply is a native mul. Without +m it becomes
; a __mulsi3 libcall.

define i32 @mul(i32 %a, i32 %b) {
; WITHM-LABEL: mul:
; WITHM: mul r{{[0-9]+}}, r{{[0-9]+}}, r{{[0-9]+}}
; NOM-LABEL: mul:
; NOM: jal {{.*}}__mulsi3
; NOM-NOT: {{[[:space:]]}}mul{{[[:space:]]}}
  %p = mul i32 %a, %b
  ret i32 %p
}
