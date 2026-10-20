#ifndef SLOW32_OPENSSL_HMAC_H
#define SLOW32_OPENSSL_HMAC_H

/* One-shot HMAC over <openssl/evp.h>'s digests, computed by the host
 * platform (runtime/crypto_mmio.c).  md NULL: a static buffer, as
 * OpenSSL's has it.  NULL when the host cannot. */

#include <openssl/evp.h>

#ifdef __cplusplus
extern "C" {
#endif

unsigned char *HMAC(const EVP_MD *evp_md, const void *key, int key_len,
                    const unsigned char *d, size_t n, unsigned char *md,
                    unsigned int *md_len);

#ifdef __cplusplus
}
#endif

#endif
