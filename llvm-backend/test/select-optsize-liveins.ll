; RUN: llc -mtriple=slow32-unknown-none -verify-machineinstrs < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; Under optsize a select is SELECT_PSEUDO, expanded by the custom inserter
; into a triangle.  The inserter copied the head block's live-ins (the
; argument registers, r29-r31) onto both new blocks; before allocation the
; arguments already live in virtual registers, and the verifier rejects a
; physical live-in on a block that is not the entry ("MBB has allocatable
; live-in").  SQLite's slow32Open was one.

; CHECK-LABEL: f:
; CHECK: b{{eq|ne}} r3, r0,
define i32 @f(i32 %a, i32 %b, i32 %c) optsize {
  %t = icmp eq i32 %a, 0
  %s = select i1 %t, i32 %b, i32 %c
  ret i32 %s
}
