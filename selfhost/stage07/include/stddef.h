/* stddef.h -- s12cc-compatible stub.  size_t / ptrdiff_t track the
 * pointer width: `unsigned long` is 8 bytes on a 64-bit host (cc-x64,
 * cc-a64 — both set ty_ptr_size=8) and 4 bytes on SLOW-32 native
 * (target_long_ty() returns TY_INT when ty_ptr_size=4).  Hardcoding
 * `unsigned int` here truncates pointer-sized fields (e.g. emit_ctx_t's
 * size_t members) on 64-bit hosts and silently corrupts struct layouts. */
#ifndef _STDDEF_H
#define _STDDEF_H

typedef unsigned long size_t;
typedef long ptrdiff_t;

#define NULL 0

#endif
