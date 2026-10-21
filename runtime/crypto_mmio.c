/* Digests, HMAC and random bytes, in OpenSSL's EVP shape, over the
 * machine's "crypto" service (docs/SPEC.md 8.17).
 *
 * The guest computes nothing: every byte goes to the host, whose platform
 * library (CommonCrypto, or OpenSSL's libcrypto) does the work -- the
 * ruling is that crypto ships as the platform's, never as code of ours.
 * A context here is a handle to one of the host's.  The service is asked
 * for on first use; a host without it (built with no platform library, or
 * a policy that denies "crypto") makes every call fail the way OpenSSL's
 * do, with 0 or NULL, and errno ENOSYS. */
#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <openssl/md5.h>
#include <openssl/rand.h>
#include <openssl/sha.h>

#include "mmio_ring.h"

struct evp_md_st {
    unsigned alg;
    int size;
    int block_size;
    const char *name;
};

struct evp_md_ctx_st {
    const EVP_MD *md;
    int handle;             /* the host's context; -1 for none */
};

static const EVP_MD md_md5    = { S32_CRYPTO_ALG_MD5,    16, 64,  "MD5" };
static const EVP_MD md_sha1   = { S32_CRYPTO_ALG_SHA1,   20, 64,  "SHA1" };
static const EVP_MD md_sha224 = { S32_CRYPTO_ALG_SHA224, 28, 64,  "SHA224" };
static const EVP_MD md_sha256 = { S32_CRYPTO_ALG_SHA256, 32, 64,  "SHA256" };
static const EVP_MD md_sha384 = { S32_CRYPTO_ALG_SHA384, 48, 128, "SHA384" };
static const EVP_MD md_sha512 = { S32_CRYPTO_ALG_SHA512, 64, 128, "SHA512" };

const EVP_MD *EVP_md5(void) { return &md_md5; }
const EVP_MD *EVP_sha1(void) { return &md_sha1; }
const EVP_MD *EVP_sha224(void) { return &md_sha224; }
const EVP_MD *EVP_sha256(void) { return &md_sha256; }
const EVP_MD *EVP_sha384(void) { return &md_sha384; }
const EVP_MD *EVP_sha512(void) { return &md_sha512; }

int EVP_MD_get_size(const EVP_MD *md) { return md ? md->size : -1; }
int EVP_MD_get_block_size(const EVP_MD *md) { return md ? md->block_size : -1; }
const char *EVP_MD_get0_name(const EVP_MD *md) { return md ? md->name : NULL; }

const EVP_MD *EVP_get_digestbyname(const char *name) {
    static const struct { const char *name; const EVP_MD *md; } names[] = {
        { "md5", &md_md5 },
        { "sha1", &md_sha1 },     { "sha-1", &md_sha1 },
        { "sha224", &md_sha224 }, { "sha-224", &md_sha224 }, { "sha2-224", &md_sha224 },
        { "sha256", &md_sha256 }, { "sha-256", &md_sha256 }, { "sha2-256", &md_sha256 },
        { "sha384", &md_sha384 }, { "sha-384", &md_sha384 }, { "sha2-384", &md_sha384 },
        { "sha512", &md_sha512 }, { "sha-512", &md_sha512 }, { "sha2-512", &md_sha512 },
    };
    if (!name) return NULL;
    for (size_t i = 0; i < sizeof names / sizeof names[0]; i++) {
        if (strcasecmp(name, names[i].name) == 0) return names[i].md;
    }
    return NULL;
}

/* ---- the service ---- */

static uint32_t base;           /* the granted opcode base */
static int state;               /* 0 not asked yet, 1 granted, -1 not there */

static int have_service(void) {
    if (state == 0) {
        static const char name[] = "crypto";
        memcpy((void *)S32_MMIO_DATA_BUFFER, name, sizeof name);
        int r = s32_mmio_request(S32_MMIO_OP_SVC_REQUEST, sizeof name, 0u, 0u);
        uint32_t result = 1;
        if (r != (int)S32_MMIO_STATUS_ERR) memcpy(&result, (const void *)S32_MMIO_DATA_BUFFER, 4);
        if (r != (int)S32_MMIO_STATUS_ERR && result == S32_SVC_OK) {
            memcpy(&base, (const unsigned char *)S32_MMIO_DATA_BUFFER + 4, 4);
            state = 1;
        } else {
            state = -1;
        }
    }
    if (state < 0) errno = ENOSYS;
    return state > 0;
}

static int request(unsigned op, unsigned length, unsigned status) {
    return s32_mmio_request(base + op, length, 0u, status);
}

/* a host context for md (an HMAC with key, if hmac): its handle, or -1 */
static int host_new(const EVP_MD *md, int hmac, const void *key, size_t keylen) {
    if (!md || !have_service()) return -1;
    if (keylen > S32_MMIO_DATA_CAPACITY) {
        errno = EINVAL;
        return -1;
    }
    if (keylen) memcpy((void *)S32_MMIO_DATA_BUFFER, key, keylen);
    int r = request(S32_CRYPTO_NEW, (unsigned)keylen, md->alg | (hmac ? S32_CRYPTO_HMAC : 0u));
    return r < 0 ? -1 : r;
}

static int host_update(int handle, const void *d, size_t n) {
    const unsigned char *p = (const unsigned char *)d;
    while (n) {
        size_t chunk = n > S32_MMIO_DATA_CAPACITY ? S32_MMIO_DATA_CAPACITY : n;
        memcpy((void *)S32_MMIO_DATA_BUFFER, p, chunk);
        if (request(S32_CRYPTO_UPDATE, (unsigned)chunk, (unsigned)handle) < 0) return 0;
        p += chunk;
        n -= chunk;
    }
    return 1;
}

/* the result, and the host context is gone either way.  expect is the
 * digest size for this algorithm: a host that returns another length
 * (or more than the buffer this interface advertises) is refused
 * before the copy. */
static int host_final(int handle, unsigned char *out, unsigned int *len, int expect) {
    int r = request(S32_CRYPTO_FINAL, 0u, (unsigned)handle);
    if (r < 0) return 0;
    if (expect < 1 || r < 1 || r > expect || (unsigned)r > S32_CRYPTO_MAX_DIGEST) {
        errno = EIO;
        return 0;
    }
    memcpy(out, (const void *)S32_MMIO_DATA_BUFFER, (size_t)r);
    if (len) *len = (unsigned)r;
    return 1;
}

static void host_free(int handle) {
    if (handle >= 0) request(S32_CRYPTO_FREE, 0u, (unsigned)handle);
}

/* ---- EVP ---- */

EVP_MD_CTX *EVP_MD_CTX_new(void) {
    EVP_MD_CTX *ctx = malloc(sizeof *ctx);
    if (ctx) {
        ctx->md = NULL;
        ctx->handle = -1;
    }
    return ctx;
}

int EVP_MD_CTX_reset(EVP_MD_CTX *ctx) {
    if (!ctx) return 0;
    host_free(ctx->handle);
    ctx->handle = -1;
    ctx->md = NULL;
    return 1;
}

void EVP_MD_CTX_free(EVP_MD_CTX *ctx) {
    if (ctx) {
        EVP_MD_CTX_reset(ctx);
        free(ctx);
    }
}

const EVP_MD *EVP_MD_CTX_get0_md(const EVP_MD_CTX *ctx) {
    return ctx ? ctx->md : NULL;
}

int EVP_DigestInit_ex(EVP_MD_CTX *ctx, const EVP_MD *type, ENGINE *impl) {
    (void)impl;
    if (!ctx) return 0;
    if (!type) type = ctx->md;
    host_free(ctx->handle);
    ctx->handle = host_new(type, 0, NULL, 0);
    if (ctx->handle < 0) return 0;
    ctx->md = type;
    return 1;
}

int EVP_DigestInit(EVP_MD_CTX *ctx, const EVP_MD *type) {
    return EVP_DigestInit_ex(ctx, type, NULL);
}

int EVP_DigestUpdate(EVP_MD_CTX *ctx, const void *d, size_t cnt) {
    if (!ctx || ctx->handle < 0) return 0;
    return host_update(ctx->handle, d, cnt);
}

int EVP_DigestFinal_ex(EVP_MD_CTX *ctx, unsigned char *md, unsigned int *s) {
    if (!ctx || ctx->handle < 0 || !md || !ctx->md) return 0;
    int ok = host_final(ctx->handle, md, s, ctx->md->size);
    ctx->handle = -1;           /* spent, as OpenSSL's is until the next Init */
    return ok;
}

int EVP_DigestFinal(EVP_MD_CTX *ctx, unsigned char *md, unsigned int *s) {
    return EVP_DigestFinal_ex(ctx, md, s);
}

int EVP_Digest(const void *data, size_t count, unsigned char *md, unsigned int *size,
               const EVP_MD *type, ENGINE *impl) {
    (void)impl;
    if (!md) return 0;
    int h = host_new(type, 0, NULL, 0);
    if (h < 0) return 0;
    if (!host_update(h, data, count)) {
        host_free(h);
        return 0;
    }
    return host_final(h, md, size, type->size);
}

unsigned char *HMAC(const EVP_MD *evp_md, const void *key, int key_len,
                    const unsigned char *d, size_t n, unsigned char *md,
                    unsigned int *md_len) {
    static unsigned char own[EVP_MAX_MD_SIZE];
    if (key_len < 0) return NULL;
    int h = host_new(evp_md, 1, key, (size_t)key_len);
    if (h < 0) return NULL;
    if (!host_update(h, d, n)) {
        host_free(h);
        return NULL;
    }
    if (!md) md = own;
    return host_final(h, md, md_len, evp_md->size) ? md : NULL;
}

static unsigned char *one_shot(const EVP_MD *type, const unsigned char *d, size_t n,
                               unsigned char *md, unsigned char *own) {
    if (!md) md = own;
    return EVP_Digest(d, n, md, NULL, type, NULL) ? md : NULL;
}

unsigned char *MD5(const unsigned char *d, size_t n, unsigned char *md) {
    static unsigned char own[MD5_DIGEST_LENGTH];
    return one_shot(&md_md5, d, n, md, own);
}
unsigned char *SHA1(const unsigned char *d, size_t n, unsigned char *md) {
    static unsigned char own[SHA_DIGEST_LENGTH];
    return one_shot(&md_sha1, d, n, md, own);
}
unsigned char *SHA224(const unsigned char *d, size_t n, unsigned char *md) {
    static unsigned char own[SHA224_DIGEST_LENGTH];
    return one_shot(&md_sha224, d, n, md, own);
}
unsigned char *SHA256(const unsigned char *d, size_t n, unsigned char *md) {
    static unsigned char own[SHA256_DIGEST_LENGTH];
    return one_shot(&md_sha256, d, n, md, own);
}
unsigned char *SHA384(const unsigned char *d, size_t n, unsigned char *md) {
    static unsigned char own[SHA384_DIGEST_LENGTH];
    return one_shot(&md_sha384, d, n, md, own);
}
unsigned char *SHA512(const unsigned char *d, size_t n, unsigned char *md) {
    static unsigned char own[SHA512_DIGEST_LENGTH];
    return one_shot(&md_sha512, d, n, md, own);
}

/* ---- random ---- */

int RAND_bytes(unsigned char *buf, int num) {
    if (num < 0 || !have_service()) return 0;
    size_t left = (size_t)num;
    while (left) {
        size_t chunk = left > S32_MMIO_DATA_CAPACITY ? S32_MMIO_DATA_CAPACITY : left;
        int r = request(S32_CRYPTO_RANDOM, (unsigned)chunk, 0u);
        if (r < 0) return 0;
        if (r != (int)chunk) {
            errno = EIO;
            return 0;
        }
        memcpy(buf, (const void *)S32_MMIO_DATA_BUFFER, chunk);
        buf += chunk;
        left -= chunk;
    }
    return 1;
}

int RAND_priv_bytes(unsigned char *buf, int num) {
    return RAND_bytes(buf, num);
}

int RAND_status(void) {
    return have_service();
}
