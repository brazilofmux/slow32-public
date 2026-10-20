#ifndef SLOW32_OPENSSL_MD5_H
#define SLOW32_OPENSSL_MD5_H

/* MD5's length and its one-shot function, over <openssl/evp.h> */

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MD5_DIGEST_LENGTH 16

unsigned char *MD5(const unsigned char *d, size_t n, unsigned char *md);

#ifdef __cplusplus
}
#endif

#endif
