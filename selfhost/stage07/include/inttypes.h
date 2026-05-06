/* inttypes.h -- s12cc-compatible stub
 * Just the printf/scanf format macros for fixed-width integer types
 * used in third-party C source.  Bare strings, like glibc.
 */
#ifndef _INTTYPES_H
#define _INTTYPES_H

#include <stdint.h>

#define PRId8   "d"
#define PRIi8   "i"
#define PRIo8   "o"
#define PRIu8   "u"
#define PRIx8   "x"
#define PRIX8   "X"

#define PRId16  "d"
#define PRIi16  "i"
#define PRIo16  "o"
#define PRIu16  "u"
#define PRIx16  "x"
#define PRIX16  "X"

#define PRId32  "d"
#define PRIi32  "i"
#define PRIo32  "o"
#define PRIu32  "u"
#define PRIx32  "x"
#define PRIX32  "X"

/* On AArch64 / SLOW-32 (LP64), int64_t is `long long`.  llprintf uses
 * `ll` length modifier. */
#define PRId64  "lld"
#define PRIi64  "lli"
#define PRIo64  "llo"
#define PRIu64  "llu"
#define PRIx64  "llx"
#define PRIX64  "llX"

/* Pointer-sized integers — same as 64-bit on this target. */
#define PRIdPTR "lld"
#define PRIuPTR "llu"
#define PRIxPTR "llx"

#endif
