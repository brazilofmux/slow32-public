#ifndef _LIMITS_H
#define _LIMITS_H

// SLOW32 Architecture Limits
// 32-bit machine with native 32-bit integers

// Char limits (8-bit)
#define CHAR_BIT    8
#define SCHAR_MIN   (-128)
#define SCHAR_MAX   127
#define UCHAR_MAX   255

// For SLOW32, char is signed by default
#define CHAR_MIN    SCHAR_MIN
#define CHAR_MAX    SCHAR_MAX

// Multi-byte character limits
#define MB_LEN_MAX  1

// Short limits (16-bit on SLOW32)
#define SHRT_MIN    (-32768)
#define SHRT_MAX    32767
#define USHRT_MAX   65535

// Int limits (32-bit on SLOW32)
#define INT_MIN     (-2147483648)
#define INT_MAX     2147483647
#define UINT_MAX    4294967295U

// Long limits (32-bit on SLOW32, same as int)
#define LONG_MIN    (-2147483648L)
#define LONG_MAX    2147483647L
#define ULONG_MAX   4294967295UL

// Long long limits (64-bit support via compiler-rt)
#define LLONG_MIN   (-9223372036854775808LL)
#define LLONG_MAX   9223372036854775807LL
#define ULLONG_MAX  18446744073709551615ULL

// POSIX limits
#define PATH_MAX    4096
#define NAME_MAX    255
#define ARG_MAX     131072
#define OPEN_MAX    256

// Implementation-defined limits
#define WORD_BIT    32          // Number of bits in a word
#define LONG_BIT    32          // Number of bits in a long

// Size limits
#define SIZE_MAX    UINT_MAX
#define SSIZE_MAX   INT_MAX

// Pointer difference limits
#define PTRDIFF_MIN INT_MIN
#define PTRDIFF_MAX INT_MAX

#endif /* _LIMITS_H */