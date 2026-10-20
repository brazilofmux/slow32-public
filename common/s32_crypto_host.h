#ifndef S32_CRYPTO_HOST_H
#define S32_CRYPTO_HOST_H

/* The host platform's message digests, HMAC and random bytes, for the
 * "crypto" service (docs/SPEC.md 8.17).  The tree carries no
 * cryptographic code, by ruling: what a guest is given is the platform's
 * own implementation -- CommonCrypto on macOS, OpenSSL's libcrypto (3.0 or
 * later) where the build found it and defined S32_CRYPTO_OPENSSL
 * (common/s32_crypto.mk).  With neither, or S32_CRYPTO_NONE, S32_CRYPTO_HOST is 0 and the service is not offered: there is
 * no fallback.
 *
 * Shared by tools/emulator/mmio_ring.c and QEMU's target/slow32/mmio.c,
 * which keeps a copy of this file. */

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#if defined(S32_CRYPTO_NONE)
#define S32_CRYPTO_HOST 0
#elif defined(__APPLE__)
#define S32_CRYPTO_HOST 1
#include <CommonCrypto/CommonDigest.h>
#include <CommonCrypto/CommonHMAC.h>
#include <CommonCrypto/CommonRandom.h>
#elif defined(S32_CRYPTO_OPENSSL)
#define S32_CRYPTO_HOST 1
#include <openssl/core_names.h>
#include <openssl/evp.h>
#include <openssl/params.h>
#include <openssl/rand.h>
#else
#define S32_CRYPTO_HOST 0
#endif

#if S32_CRYPTO_HOST

#include <errno.h>
#include <stdlib.h>
#include <string.h>

/* The algorithms, by the guest's numbers (common/mmio_ring_layout.h) */
#define S32_CRYPTO_HOST_MD5    1u
#define S32_CRYPTO_HOST_SHA1   2u
#define S32_CRYPTO_HOST_SHA224 3u
#define S32_CRYPTO_HOST_SHA256 4u
#define S32_CRYPTO_HOST_SHA384 5u
#define S32_CRYPTO_HOST_SHA512 6u

/* the digest's size in bytes; 0 for an algorithm there is not */
static inline uint32_t s32_crypto_size(uint32_t alg) {
    switch (alg) {
    case S32_CRYPTO_HOST_MD5:    return 16;
    case S32_CRYPTO_HOST_SHA1:   return 20;
    case S32_CRYPTO_HOST_SHA224: return 28;
    case S32_CRYPTO_HOST_SHA256: return 32;
    case S32_CRYPTO_HOST_SHA384: return 48;
    case S32_CRYPTO_HOST_SHA512: return 64;
    default:                     return 0;
    }
}

#if defined(__APPLE__)

/* MD5 and SHA-1 are deprecated in CommonCrypto's headers; they are still
 * what a program asks for, and the platform still answers */
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"

typedef struct {
    uint32_t alg;
    bool hmac;
    union {
        CC_MD5_CTX md5;
        CC_SHA1_CTX sha1;
        CC_SHA256_CTX sha256;       /* SHA-224 too */
        CC_SHA512_CTX sha512;       /* SHA-384 too */
        CCHmacContext mac;
    } u;
} s32_crypto_ctx_t;

static inline CCHmacAlgorithm s32_cc_hmac_alg(uint32_t alg) {
    switch (alg) {
    case S32_CRYPTO_HOST_MD5:    return kCCHmacAlgMD5;
    case S32_CRYPTO_HOST_SHA1:   return kCCHmacAlgSHA1;
    case S32_CRYPTO_HOST_SHA224: return kCCHmacAlgSHA224;
    case S32_CRYPTO_HOST_SHA256: return kCCHmacAlgSHA256;
    case S32_CRYPTO_HOST_SHA384: return kCCHmacAlgSHA384;
    default:                     return kCCHmacAlgSHA512;
    }
}

/* 0, or an errno: EINVAL for an algorithm there is not, ENOMEM */
static inline int s32_crypto_new(uint32_t alg, bool hmac, const void *key, size_t keylen,
                                 s32_crypto_ctx_t **out) {
    if (!s32_crypto_size(alg)) return EINVAL;
    s32_crypto_ctx_t *c = (s32_crypto_ctx_t *)calloc(1, sizeof *c);
    if (!c) return ENOMEM;
    c->alg = alg;
    c->hmac = hmac;
    if (hmac) {
        CCHmacInit(&c->u.mac, s32_cc_hmac_alg(alg), key, keylen);
    } else {
        switch (alg) {
        case S32_CRYPTO_HOST_MD5:    CC_MD5_Init(&c->u.md5); break;
        case S32_CRYPTO_HOST_SHA1:   CC_SHA1_Init(&c->u.sha1); break;
        case S32_CRYPTO_HOST_SHA224: CC_SHA224_Init(&c->u.sha256); break;
        case S32_CRYPTO_HOST_SHA256: CC_SHA256_Init(&c->u.sha256); break;
        case S32_CRYPTO_HOST_SHA384: CC_SHA384_Init(&c->u.sha512); break;
        default:                     CC_SHA512_Init(&c->u.sha512); break;
        }
    }
    *out = c;
    return 0;
}

static inline int s32_crypto_update(s32_crypto_ctx_t *c, const void *p, size_t n) {
    if (c->hmac) {
        CCHmacUpdate(&c->u.mac, p, n);
        return 0;
    }
    CC_LONG len = (CC_LONG)n;
    switch (c->alg) {
    case S32_CRYPTO_HOST_MD5:    CC_MD5_Update(&c->u.md5, p, len); break;
    case S32_CRYPTO_HOST_SHA1:   CC_SHA1_Update(&c->u.sha1, p, len); break;
    case S32_CRYPTO_HOST_SHA224: CC_SHA224_Update(&c->u.sha256, p, len); break;
    case S32_CRYPTO_HOST_SHA256: CC_SHA256_Update(&c->u.sha256, p, len); break;
    case S32_CRYPTO_HOST_SHA384: CC_SHA384_Update(&c->u.sha512, p, len); break;
    default:                     CC_SHA512_Update(&c->u.sha512, p, len); break;
    }
    return 0;
}

/* The digest or MAC into out (room for 64); the context is spent */
static inline int s32_crypto_final(s32_crypto_ctx_t *c, uint8_t *out, uint32_t *len) {
    *len = s32_crypto_size(c->alg);
    if (c->hmac) {
        CCHmacFinal(&c->u.mac, out);
        return 0;
    }
    switch (c->alg) {
    case S32_CRYPTO_HOST_MD5:    CC_MD5_Final(out, &c->u.md5); break;
    case S32_CRYPTO_HOST_SHA1:   CC_SHA1_Final(out, &c->u.sha1); break;
    case S32_CRYPTO_HOST_SHA224: CC_SHA224_Final(out, &c->u.sha256); break;
    case S32_CRYPTO_HOST_SHA256: CC_SHA256_Final(out, &c->u.sha256); break;
    case S32_CRYPTO_HOST_SHA384: CC_SHA384_Final(out, &c->u.sha512); break;
    default:                     CC_SHA512_Final(out, &c->u.sha512); break;
    }
    return 0;
}

static inline void s32_crypto_free(s32_crypto_ctx_t *c) {
    if (c) {
        memset(c, 0, sizeof *c);    /* a key schedule does not outlive it */
        free(c);
    }
}

static inline int s32_crypto_random(void *p, size_t n) {
    return CCRandomGenerateBytes(p, n) == kCCSuccess ? 0 : EIO;
}

#pragma clang diagnostic pop

#else /* OpenSSL */

typedef struct {
    uint32_t alg;
    bool hmac;
    EVP_MD_CTX *md;
    EVP_MAC_CTX *mac;
} s32_crypto_ctx_t;

static inline const char *s32_ossl_name(uint32_t alg) {
    switch (alg) {
    case S32_CRYPTO_HOST_MD5:    return "MD5";
    case S32_CRYPTO_HOST_SHA1:   return "SHA1";
    case S32_CRYPTO_HOST_SHA224: return "SHA224";
    case S32_CRYPTO_HOST_SHA256: return "SHA256";
    case S32_CRYPTO_HOST_SHA384: return "SHA384";
    default:                     return "SHA512";
    }
}

static inline void s32_crypto_free(s32_crypto_ctx_t *c) {
    if (c) {
        EVP_MD_CTX_free(c->md);
        EVP_MAC_CTX_free(c->mac);
        free(c);
    }
}

/* 0, or an errno: EINVAL for an algorithm there is not, ENOMEM, EIO when
 * the library refuses (a FIPS configuration without MD5, say) */
static inline int s32_crypto_new(uint32_t alg, bool hmac, const void *key, size_t keylen,
                                 s32_crypto_ctx_t **out) {
    if (!s32_crypto_size(alg)) return EINVAL;
    s32_crypto_ctx_t *c = (s32_crypto_ctx_t *)calloc(1, sizeof *c);
    if (!c) return ENOMEM;
    c->alg = alg;
    c->hmac = hmac;
    if (hmac) {
        /* an empty key is a key: EVP_MAC_init takes NULL as "no new key" */
        static const unsigned char none = 0;
        EVP_MAC *m = EVP_MAC_fetch(NULL, "HMAC", NULL);
        c->mac = m ? EVP_MAC_CTX_new(m) : NULL;
        EVP_MAC_free(m);
        OSSL_PARAM params[2] = {
            OSSL_PARAM_construct_utf8_string(OSSL_MAC_PARAM_DIGEST, (char *)s32_ossl_name(alg), 0),
            OSSL_PARAM_construct_end(),
        };
        if (!c->mac || !EVP_MAC_init(c->mac, key ? (const unsigned char *)key : &none, keylen, params)) {
            s32_crypto_free(c);
            return EIO;
        }
    } else {
        EVP_MD *md = EVP_MD_fetch(NULL, s32_ossl_name(alg), NULL);
        c->md = EVP_MD_CTX_new();
        int ok = md && c->md && EVP_DigestInit_ex(c->md, md, NULL);
        EVP_MD_free(md);
        if (!ok) {
            s32_crypto_free(c);
            return EIO;
        }
    }
    *out = c;
    return 0;
}

static inline int s32_crypto_update(s32_crypto_ctx_t *c, const void *p, size_t n) {
    int ok = c->hmac ? EVP_MAC_update(c->mac, (const unsigned char *)p, n)
                     : EVP_DigestUpdate(c->md, p, n);
    return ok ? 0 : EIO;
}

static inline int s32_crypto_final(s32_crypto_ctx_t *c, uint8_t *out, uint32_t *len) {
    if (c->hmac) {
        size_t got = 0;
        if (!EVP_MAC_final(c->mac, out, &got, 64)) return EIO;
        *len = (uint32_t)got;
        return 0;
    }
    unsigned int got = 0;
    if (!EVP_DigestFinal_ex(c->md, out, &got)) return EIO;
    *len = got;
    return 0;
}

static inline int s32_crypto_random(void *p, size_t n) {
    return n > (size_t)INT32_MAX || RAND_bytes((unsigned char *)p, (int)n) != 1 ? EIO : 0;
}

#endif /* platform */
#endif /* S32_CRYPTO_HOST */
#endif /* S32_CRYPTO_HOST_H */
