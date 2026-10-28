; RUN: llc -O2 -mtriple=slow32-unknown-none -verify-machineinstrs < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; The constant 0 is r0.  It was "addi rN, r0, 0": a register for every
; compare against zero and every zero store, and in a function that calls,
; a callee-saved register set in the entry block and saved by the prologue.

; CHECK-LABEL: cmp0:
; CHECK-NOT: addi r{{[0-9]+}}, r0, 0
; CHECK: beq r3, r0,
define i32 @cmp0(i32 %x) nounwind {
  %c = icmp eq i32 %x, 0
  br i1 %c, label %z, label %nz
z:
  ret i32 7
nz:
  ret i32 9
}

; CHECK-LABEL: store0:
; CHECK-NOT: addi r{{[0-9]+}}, r0, 0
; CHECK: stw r3+0, r0
; CHECK: stw r3+4, r0
define void @store0(ptr %p) nounwind {
  store i32 0, ptr %p
  %q = getelementptr i32, ptr %p, i32 1
  store i32 0, ptr %q
  ret void
}

; Two zero tests on either side of a call: no callee-saved register holds
; the zero across it, so the frame saves lr alone.  (The early exit shares
; its return block with a path through the call, so ShrinkWrap keeps the
; prologue in the entry block here.)
declare ptr @get(i32)
; CHECK-LABEL: around:
; CHECK: addi sp, sp, -16
; CHECK-NEXT: stw sp+0, lr
; CHECK-NEXT: beq r3, r0,
; CHECK-NOT: r11
; CHECK: jal r31, get
; CHECK-NEXT: beq r1, r0,
define i32 @around(i32 %n) nounwind {
entry:
  %c = icmp eq i32 %n, 0
  br i1 %c, label %out, label %call
call:
  %p = call ptr @get(i32 %n)
  %d = icmp eq ptr %p, null
  br i1 %d, label %out, label %use
use:
  %v = load i32, ptr %p
  ret i32 %v
out:
  ret i32 -1
}
