; RUN: llc -mtriple=slow32-unknown-none < %s | FileCheck %s

; SLOW-32 is single-hart: atomics lower to plain loads/stores (AtomicExpand
; marks them NotAtomic), fences disappear, and thread_local goes through
; emulated TLS (__emutls_get_address).

define i32 @atomic_load32(ptr %p) {
; CHECK-LABEL: atomic_load32:
; CHECK: ldw
; CHECK-NOT: __atomic
; CHECK: jalr r0, r31, 0
entry:
  %v = load atomic i32, ptr %p seq_cst, align 4
  ret i32 %v
}

define void @atomic_store32(ptr %p, i32 %v) {
; CHECK-LABEL: atomic_store32:
; CHECK: stw
; CHECK-NOT: __atomic
; CHECK: jalr r0, r31, 0
entry:
  store atomic i32 %v, ptr %p seq_cst, align 4
  ret void
}

define i32 @atomic_rmw_add(ptr %p, i32 %v) {
; CHECK-LABEL: atomic_rmw_add:
; CHECK: ldw
; CHECK: add
; CHECK: stw
; CHECK-NOT: __atomic
entry:
  %old = atomicrmw add ptr %p, i32 %v seq_cst, align 4
  ret i32 %old
}

define i32 @atomic_cmpxchg(ptr %p, i32 %cmp, i32 %new) {
; CHECK-LABEL: atomic_cmpxchg:
; CHECK: ldw
; CHECK-NOT: __atomic
entry:
  %pair = cmpxchg ptr %p, i32 %cmp, i32 %new seq_cst seq_cst, align 4
  %old = extractvalue { i32, i1 } %pair, 0
  ret i32 %old
}

define i64 @atomic_load64(ptr %p) {
; CHECK-LABEL: atomic_load64:
; CHECK: ldw
; CHECK: ldw
; CHECK-NOT: __atomic
entry:
  %v = load atomic i64, ptr %p seq_cst, align 8
  ret i64 %v
}

; Fences are no-ops on a single-hart machine.
define void @seq_cst_barrier() {
; CHECK-LABEL: seq_cst_barrier:
; CHECK-NOT: fence
; CHECK: jalr r0, r31, 0
entry:
  fence seq_cst
  ret void
}

; thread_local access compiles to an __emutls_get_address call.
@tls_var = thread_local global i32 7, align 4

define i32 @read_tls() {
; CHECK-LABEL: read_tls:
; CHECK: __emutls_v.tls_var
; CHECK: jal r31, __emutls_get_address
; CHECK: ldw
entry:
  %v = load i32, ptr @tls_var, align 4
  ret i32 %v
}

; The control block and template are emitted with the right layout:
; {size, align, ptr, templ} plus a template holding the initializer.
; CHECK: __emutls_v.tls_var:
; CHECK: .word 4
; CHECK: .word 4
; CHECK: .word 0
; CHECK: .word __emutls_t.tls_var
; CHECK: __emutls_t.tls_var:
; CHECK: .word 7
