; RUN: llc -mtriple=slow32-unknown-none -O0 < %s | FileCheck %s
; RUN: llc -mtriple=slow32-unknown-none -O2 < %s | FileCheck %s

; An 'm'-constrained stack local puts a FrameIndex operand inside an
; INLINEASM instruction. INLINEASM is variadic: indexing its MCInstrDesc
; operand list by the FI's position is out of bounds, and the positional
; shape heuristics in eliminateFrameIndex would misread the surrounding
; constraint-flag operands. The FI must simply be materialised as an
; address register.

define i32 @asm_mem_local() {
; CHECK-LABEL: asm_mem_local:
; CHECK: #APP
; CHECK: #NO_APP
entry:
  %local = alloca i32, align 4
  store i32 42, ptr %local
  call void asm sideeffect "# touch $0", "*m"(ptr elementtype(i32) %local)
  %v = load i32, ptr %local
  ret i32 %v
}
