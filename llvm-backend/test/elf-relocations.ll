; RUN: llc -mtriple=slow32-unknown-none -filetype=obj < %s -o %t.o
; RUN: llvm-readobj --expand-relocs -r %t.o | FileCheck %s
; REQUIRES: slow32-registered-target

; References to external symbols must emit real relocations (HI20=2 for the LUI
; %hi and LO12=3 for the ADDI %lo), matching docs/file-formats.md. Previously the
; ELF writer returned type 0 (NONE) for every fixup, so a linker would never
; patch the address and the program would dereference address 0.

@g = external global i32

define i32 @f() {
; CHECK: Relocations [
; CHECK: Offset: 0x0
; CHECK-NEXT: Type: {{.*}}(2)
; CHECK: Offset: 0x4
; CHECK-NEXT: Type: {{.*}}(3)
  %v = load i32, ptr @g
  ret i32 %v
}
