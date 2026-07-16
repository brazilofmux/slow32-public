; RUN: llc -mtriple=slow32-unknown-none -filetype=obj < %s -o %t.o
; RUN: llvm-objdump -s -j .text %t.o | FileCheck %s
; REQUIRES: slow32-registered-target

; Floating-point and 64-bit-multiply instructions are selectable, so the MC code
; emitter must encode them rather than hitting llvm_unreachable on -filetype=obj.
; Encodings follow the R-type layout opcode | rd<<7 | rs1<<15 | rs2<<20, with .D
; register pairs encoding as their low register.

; fadd.s r1, r3, r4 = 0x53 | 1<<7 | 3<<15 | 4<<20 = 0x004180D3 (little-endian d3804100)
; CHECK: Contents of section .text:
; CHECK-NEXT: 0000 d3804100
define float @fa(float %a, float %b) {
  %r = fadd float %a, %b
  ret float %r
}
