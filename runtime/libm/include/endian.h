/* What musl's libm.h asks of <endian.h>: SLOW-32 is little-endian. */
#ifndef _S32_LIBM_ENDIAN_H
#define _S32_LIBM_ENDIAN_H
#define __LITTLE_ENDIAN 1234
#define __BIG_ENDIAN 4321
#define __BYTE_ORDER __LITTLE_ENDIAN
#endif
