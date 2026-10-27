; RUN: llc -O2 -mtriple=slow32-unknown-none -verify-machineinstrs < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; An indirect call expands after allocation to JALR_CALLR, whose explicit
; def of the link register r31 sat beside the Defs list's implicit dead
; r31.  The explicit one was live, and the verifier rejects a register both
; dead and live on one instruction ("Dead def has a live def of the same
; register"), stopping at the first such function in ten runtime files.

; CHECK-LABEL: callp:
; CHECK: jalr lr, r{{[0-9]+}}, 0
define i32 @callp(ptr %f, i32 %x) {
  %r = call i32 %f(i32 %x)
  %s = add i32 %r, 1
  ret i32 %s
}
