// runtime/include/stddef.h (SLOW-32)
#ifndef _SLOW32_STDDEF_H_
#define _SLOW32_STDDEF_H_

// Basic type definitions for SLOW-32
typedef unsigned int size_t;
typedef int ssize_t;    // POSIX (sys/types.h there); kept here beside size_t
typedef int ptrdiff_t;
#ifndef __cplusplus
typedef int wchar_t;            // a keyword in C++
#endif

// The strictest alignment of any scalar type (C11, C++11)
typedef struct { long long __ll; long double __ld; } max_align_t;

#ifdef __cplusplus
#define NULL __null
#else
#define NULL ((void*)0)
#endif

// Use compiler's offsetof
#define offsetof(type, member) __builtin_offsetof(type, member)

#endif