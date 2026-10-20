#ifndef SLOW32_OPENSSL_EVP_H
#define SLOW32_OPENSSL_EVP_H

/* The message-digest part of OpenSSL's EVP interface, for SLOW-32.  This is
 * not OpenSSL: no cipher, no key, no TLS.  It is the shape programs call,
 * over the machine's "crypto" service, and the digests are computed by the
 * host platform's own library (CommonCrypto, or OpenSSL's libcrypto) --
 * there is no cryptographic code in the guest (runtime/crypto_mmio.c,
 * docs/SPEC.md 8.17).  Where the host offers no such service, every call
 * fails as OpenSSL's do: 0, or NULL.
 *
 * MD5, SHA-1, SHA-224, SHA-256, SHA-384, SHA-512.  OPENSSL_VERSION_NUMBER
 * is not defined: code that tests it takes its oldest path, which is the
 * one this supports (EVP_get_digestbyname, not EVP_MD_fetch). */

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define EVP_MAX_MD_SIZE 64
#define EVP_MAX_BLOCK_LENGTH 32

typedef struct evp_md_st EVP_MD;
typedef struct evp_md_ctx_st EVP_MD_CTX;
typedef struct engine_st ENGINE;

const EVP_MD *EVP_md5(void);
const EVP_MD *EVP_sha1(void);
const EVP_MD *EVP_sha224(void);
const EVP_MD *EVP_sha256(void);
const EVP_MD *EVP_sha384(void);
const EVP_MD *EVP_sha512(void);

/* "md5", "sha1", "sha256", "SHA-256", "SHA2-256" and the like, any case;
 * NULL for another */
const EVP_MD *EVP_get_digestbyname(const char *name);

int EVP_MD_get_size(const EVP_MD *md);
int EVP_MD_get_block_size(const EVP_MD *md);
const char *EVP_MD_get0_name(const EVP_MD *md);
#define EVP_MD_size EVP_MD_get_size
#define EVP_MD_block_size EVP_MD_get_block_size

EVP_MD_CTX *EVP_MD_CTX_new(void);
void EVP_MD_CTX_free(EVP_MD_CTX *ctx);
int EVP_MD_CTX_reset(EVP_MD_CTX *ctx);
#define EVP_MD_CTX_create() EVP_MD_CTX_new()
#define EVP_MD_CTX_destroy(ctx) EVP_MD_CTX_free((ctx))
const EVP_MD *EVP_MD_CTX_get0_md(const EVP_MD_CTX *ctx);
#define EVP_MD_CTX_md EVP_MD_CTX_get0_md

/* type NULL reuses the context's digest; impl is ignored */
int EVP_DigestInit_ex(EVP_MD_CTX *ctx, const EVP_MD *type, ENGINE *impl);
int EVP_DigestInit(EVP_MD_CTX *ctx, const EVP_MD *type);
int EVP_DigestUpdate(EVP_MD_CTX *ctx, const void *d, size_t cnt);
int EVP_DigestFinal_ex(EVP_MD_CTX *ctx, unsigned char *md, unsigned int *s);
int EVP_DigestFinal(EVP_MD_CTX *ctx, unsigned char *md, unsigned int *s);
int EVP_Digest(const void *data, size_t count, unsigned char *md, unsigned int *size,
               const EVP_MD *type, ENGINE *impl);

/* Nothing to set up or tear down */
#define OpenSSL_add_all_digests() ((void)0)
#define EVP_cleanup() ((void)0)

#ifdef __cplusplus
}
#endif

#endif
