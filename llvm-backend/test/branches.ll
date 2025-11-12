; RUN: llc -mtriple=slow32-unknown-none -verify-machineinstrs < %s | FileCheck %s

; Signed greater-than should materialise the false edge with BLE
define i32 @sgt(i32 %a, i32 %b) {
; CHECK-LABEL: sgt:
; CHECK: ble r3, r4,
entry:
  %cmp = icmp sgt i32 %a, %b
  br i1 %cmp, label %then, label %else
then:
  ret i32 1
else:
  ret i32 0
}

; Signed less-or-equal should materialise the false edge with BGT
define i32 @sle(i32 %a, i32 %b) {
; CHECK-LABEL: sle:
; CHECK: bgt r3, r4,
entry:
  %cmp = icmp sle i32 %a, %b
  br i1 %cmp, label %then, label %else
then:
  ret i32 42
else:
  ret i32 0
}

; Unsigned less-than should materialise the false edge with BGEU
define i32 @ult(i32 %a, i32 %b) {
; CHECK-LABEL: ult:
; CHECK: bgeu r3, r4,
entry:
  %cmp = icmp ult i32 %a, %b
  br i1 %cmp, label %then, label %else
then:
  ret i32 7
else:
  ret i32 0
}

; Unsigned greater-or-equal should materialise the false edge with BLTU
define i32 @uge(i32 %a, i32 %b) {
; CHECK-LABEL: uge:
; CHECK: bltu r3, r4,
entry:
  %cmp = icmp uge i32 %a, %b
  br i1 %cmp, label %then, label %else
then:
  ret i32 5
else:
  ret i32 0
}

; Unsigned greater-than should materialise the false edge with BLEU
define i32 @ugt(i32 %a, i32 %b) {
; CHECK-LABEL: ugt:
; CHECK: bleu r3, r4,
entry:
  %cmp = icmp ugt i32 %a, %b
  br i1 %cmp, label %then, label %else
then:
  ret i32 9
else:
  ret i32 0
}

; Unsigned less-or-equal should materialise the false edge with BGTU
define i32 @ule(i32 %a, i32 %b) {
; CHECK-LABEL: ule:
; CHECK: bgtu r3, r4,
entry:
  %cmp = icmp ule i32 %a, %b
  br i1 %cmp, label %then, label %else
then:
  ret i32 3
else:
  ret i32 0
}
