/* stddef.h -- s12cc-compatible stub.  size_t / ptrdiff_t track the
 * pointer width: `unsigned long` is 8 bytes on a 64-bit host (cc-x64,
 * cc-a64 — both set ty_ptr_size=8) and 4 bytes on SLOW-32 native
 * (target_long_ty() returns TY_INT when ty_ptr_size=4).  Hardcoding
 * `unsigned int` here truncates pointer-sized fields (e.g. emit_ctx_t's
 * size_t members) on 64-bit hosts and silently corrupts struct layouts. */
#ifndef _STDDEF_H
#define _STDDEF_H

/* On SLOW-32 they are unsigned int and int, as clang's target has them
 * (__SIZE_TYPE__, __PTRDIFF_TYPE__): the same size as unsigned long and
 * long, but other types, which _Generic tells apart. */
#if __SIZEOF_POINTER__ == 8
typedef unsigned long size_t;
typedef long ptrdiff_t;
#else
typedef unsigned int size_t;
typedef int ptrdiff_t;
#endif

/* wide characters: L'x' and L"" are of this type (6.4.4.4, 6.4.5) --
 * unsigned int on SLOW-32, as clang's target has __WCHAR_TYPE__ */
#ifndef _WCHAR_T_DEFINED
#define _WCHAR_T_DEFINED
#if __SIZEOF_POINTER__ == 8
typedef int wchar_t;
#else
typedef unsigned int wchar_t;
#endif
#endif

/* the type of greatest fundamental alignment (6.2.8p2) */
typedef struct { long long __ll; double __d; } max_align_t;

#define NULL 0

/* The compiler's own: a program's #define offsetof is honoured too (the
 * lexer's offsetof keyword is a fallback for code without this header),
 * and SQLite's `#ifndef offsetof` keeps it from defining its own. */
#define offsetof(type, member) __builtin_offsetof(type, member)

#endif
