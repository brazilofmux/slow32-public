#ifndef SLOW32_OPENSSL_SHA_H
#define SLOW32_OPENSSL_SHA_H

/* The digest lengths, and the one-shot SHA functions, over <openssl/evp.h>
 * (runtime/crypto_mmio.c).  md NULL: a static buffer.  NULL when the host
 * cannot. */

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define SHA_DIGEST_LENGTH    20
#define SHA224_DIGEST_LENGTH 28
#define SHA256_DIGEST_LENGTH 32
#define SHA384_DIGEST_LENGTH 48
#define SHA512_DIGEST_LENGTH 64

unsigned char *SHA1(const unsigned char *d, size_t n, unsigned char *md);
unsigned char *SHA224(const unsigned char *d, size_t n, unsigned char *md);
unsigned char *SHA256(const unsigned char *d, size_t n, unsigned char *md);
unsigned char *SHA384(const unsigned char *d, size_t n, unsigned char *md);
unsigned char *SHA512(const unsigned char *d, size_t n, unsigned char *md);

#ifdef __cplusplus
}
#endif

#endif
