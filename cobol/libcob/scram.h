/* scram.h -- SCRAM-SHA-256 for the PostgreSQL client (pgwire.c): SHA-256
 * (FIPS 180-4), HMAC-SHA-256 (RFC 2104), PBKDF2-HMAC-SHA-256 (RFC 8018),
 * base64 (RFC 4648), and the client side of SCRAM (RFC 5802, RFC 7677).
 * Plain C: built for SLOW-32 into the ESQL runtime, and for the host by
 * tests/scram_test.c, which checks it against the RFCs' vectors. */
#ifndef S32_SCRAM_H
#define S32_SCRAM_H

#include <stddef.h>

typedef struct {
    unsigned int h[8];
    unsigned char buf[64];
    unsigned long long len;          /* bytes hashed so far */
    int n;                           /* bytes in buf */
} sha256_ctx;

void sha256_init(sha256_ctx *c);
void sha256_update(sha256_ctx *c, const void *p, size_t n);
void sha256_final(sha256_ctx *c, unsigned char out[32]);
void sha256(const void *p, size_t n, unsigned char out[32]);
void hmac_sha256(const void *key, size_t klen, const void *msg, size_t mlen, unsigned char out[32]);
void pbkdf2_sha256(const void *pw, size_t pwlen, const void *salt, size_t slen,
                   unsigned iterations, unsigned char *out, size_t outlen);
/* base64: the encoded length, NUL-terminated; or the decoded length, -1 if malformed */
int b64_encode(const unsigned char *p, int n, char *out, int outsz);
int b64_decode(const char *s, int n, unsigned char *out, int outsz);

typedef struct {
    char client_first_bare[256];     /* n=user,r=cnonce */
    char auth_message[1024];         /* client-first-bare,server-first,client-final-without-proof */
    unsigned char server_signature[32];
} scram_state;

/* client-first-message, "n,,n=<user>,r=<cnonce>", into out; its length or -1 */
int scram_client_first(scram_state *s, const char *user, const char *cnonce, char *out, int outsz);
/* from the server-first-message, the client-final-message into out; its
 * length, or -1 if the server's message is malformed or its nonce does not
 * extend ours */
int scram_client_final(scram_state *s, const char *password, const char *server_first, int sflen,
                       char *out, int outsz);
/* the server-final-message: 1 if its signature is the one we computed */
int scram_verify_server(const scram_state *s, const char *server_final, int len);

#endif
