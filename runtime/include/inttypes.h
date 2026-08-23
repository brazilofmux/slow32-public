#ifndef _INTTYPES_H
#define _INTTYPES_H

/* Minimal C99 inttypes: the format macros for a 32-bit int world. */

#include <stdint.h>

#define PRId8  "d"
#define PRId16 "d"
#define PRId32 "d"
#define PRId64 "lld"
#define PRIu8  "u"
#define PRIu16 "u"
#define PRIu32 "u"
#define PRIu64 "llu"
#define PRIx8  "x"
#define PRIx16 "x"
#define PRIx32 "x"
#define PRIx64 "llx"
#define PRIX32 "X"
#define PRIX64 "llX"

#define SCNd32 "d"
#define SCNu32 "u"
#define SCNx32 "x"

typedef long long intmax_t;
typedef unsigned long long uintmax_t;

#endif /* _INTTYPES_H */
