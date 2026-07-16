; RUN: llc -mtriple=slow32-unknown-none -filetype=obj < %s -o %t.o
; RUN: llvm-objdump -s -j .text %t.o | FileCheck %s
; REQUIRES: slow32-registered-target

; Conditional branches resolved at the MC layer must encode a PC+4-relative
; displacement (target - (PC + 4)), matching the hardware (next_pc = PC+4+imm)
; and the s32-ld linker. Here .LBB0_1 is at 0x04 and the blt at 0x08, so the
; encoded offset is 0x04 - (0x08 + 4) = -8, giving the branch word 0xFE308CCA
; (little-endian ca8c30fe). A wrong PC-relative encoding would emit -4 and the
; branch would jump back onto itself.

define i32 @loop(i32 %n) {
; CHECK: Contents of section .text:
; CHECK-NEXT: 0000 90000000 90801000 ca8c30fe
entry:
  br label %head
head:
  %i = phi i32 [0, %entry], [%i2, %head]
  %i2 = add i32 %i, 1
  %c = icmp slt i32 %i2, %n
  br i1 %c, label %head, label %exit
exit:
  ret i32 %i2
}
