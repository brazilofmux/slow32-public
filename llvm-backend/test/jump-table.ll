; RUN: llc -mtriple=slow32-unknown-none -O2 < %s | FileCheck %s
; REQUIRES: slow32-registered-target
;
; Dense switches lower via an expanded jump table: LOAD_ADDR of .LJTI*,
; scaled index, word load, and BRIND (jalr). Range check uses bgtu.

define i32 @sw(i32 %x) {
; CHECK-LABEL: sw:
; CHECK: bgtu
; CHECK: slli
; CHECK: lui {{r[0-9]+}}, %hi(.LJTI
; CHECK: addi {{r[0-9]+}}, {{r[0-9]+}}, %lo(.LJTI
; CHECK: ldw
; CHECK: jalr r0, {{r[0-9]+}}, 0
; CHECK: .LJTI0_0:
; CHECK: .word
entry:
  switch i32 %x, label %def [
    i32 0, label %a
    i32 1, label %b
    i32 2, label %c
    i32 3, label %d
    i32 4, label %e
    i32 5, label %f
    i32 6, label %g
    i32 7, label %h
  ]
a: ret i32 10
b: ret i32 20
c: ret i32 30
d: ret i32 40
e: ret i32 50
f: ret i32 60
g: ret i32 70
h: ret i32 80
def: ret i32 -1
}
