// runtime/include/stdarg.h  (SLOW-32)
#ifndef _SLOW32_STDARG_H_
#define _SLOW32_STDARG_H_

#include <stdint.h>

// SLOW-32 ABI: varargs are a linear block of 32-bit stack slots.
// va_list is just a pointer into that block.
typedef __builtin_va_list va_list;

// Use the compiler builtins so Clang/LLVM emits llvm.va_start / va_end / va_arg.
// Our backend already lowers those intrinsics to the SLOW-32 ABI:
//
//   - va_start(ap, last): we lower to  ap = (fp + CurrentFrameSize)
//   - va_arg(ap, T)     : load *(T*)ap; ap += 4 (for 32-bit types)
//   - va_end(ap)        : no-op
//
#define va_start(ap, last) __builtin_va_start(ap, last)
#define va_end(ap)         __builtin_va_end(ap)
#define va_copy(dst, src)  ((dst) = (src))
#define va_arg(ap, type)   __builtin_va_arg(ap, type)

#endif