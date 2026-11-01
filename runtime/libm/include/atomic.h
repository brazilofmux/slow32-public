/* What musl's fma.c takes from its private atomic.h: a_clz_64, the
 * leading zero count of a nonzero 64-bit value. */
#ifndef _S32_LIBM_ATOMIC_H
#define _S32_LIBM_ATOMIC_H
#include <stdint.h>
static inline int a_clz_64(uint64_t x)
{
	int n = 0;
	if (!(x >> 32)) { n += 32; x <<= 32; }
	if (!(x >> 48)) { n += 16; x <<= 16; }
	if (!(x >> 56)) { n += 8; x <<= 8; }
	if (!(x >> 60)) { n += 4; x <<= 4; }
	if (!(x >> 62)) { n += 2; x <<= 2; }
	if (!(x >> 63)) { n += 1; }
	return n;
}
#endif
