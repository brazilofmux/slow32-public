// runtime/include/stddef.h (SLOW-32)
#ifndef _SLOW32_STDDEF_H_
#define _SLOW32_STDDEF_H_

// Basic type definitions for SLOW-32
typedef unsigned int size_t;
typedef int ptrdiff_t;
typedef int wchar_t;

#define NULL ((void*)0)

// Use compiler's offsetof
#define offsetof(type, member) __builtin_offsetof(type, member)

#endif