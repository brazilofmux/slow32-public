; RUN: llc -O2 -mtriple=slow32-unknown-none -verify-machineinstrs < %s | FileCheck %s
; REQUIRES: slow32-registered-target

; An address used only as the base of loads and stores at one offset is
; "lui %hi(sym)" with %lo(sym) in each access -- not lui/addi and +0.  A
; word of a static struct (GlobalOpt splits one into a global per field)
; was three instructions; it is two, and a read-modify-write shares the lui.

@w = internal global i32 0
@h = internal global i16 0
@b = internal global i8 0
@p = internal global ptr null

; CHECK-LABEL: rmw:
; CHECK: lui [[R:r[0-9]+]], %hi(w)
; CHECK-NEXT: ldw [[V:r[0-9]+]], [[R]]+%lo(w)
; CHECK: stw [[R]]+%lo(w), {{r[0-9]+}}
; CHECK-NOT: addi
; CHECK: jalr r0, r31, 0
define void @rmw(i32 %x) nounwind {
  %v = load i32, ptr @w
  %s = add i32 %v, %x
  store i32 %s, ptr @w
  ret void
}

; CHECK-LABEL: narrow:
; CHECK: lui [[H:r[0-9]+]], %hi(h)
; CHECK-NEXT: ldhu {{r[0-9]+}}, [[H]]+%lo(h)
; CHECK: lui [[B:r[0-9]+]], %hi(b)
; CHECK-NEXT: stb [[B]]+%lo(b), {{r[0-9]+}}
define i32 @narrow(i8 %c) nounwind {
  %v = load i16, ptr @h
  store i8 %c, ptr @b
  %z = zext i16 %v to i32
  ret i32 %z
}

; The address itself escapes (stored as a value): it stays lui/addi.
; CHECK-LABEL: escapes:
; CHECK: lui [[A:r[0-9]+]], %hi(w)
; CHECK-NEXT: addi [[A]], [[A]], %lo(w)
; CHECK: stw {{r[0-9]+}}+%lo(p), [[A]]
define void @escapes() nounwind {
  store ptr @w, ptr @p
  ret void
}
