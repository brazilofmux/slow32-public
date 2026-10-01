/* scram.c -- see scram.h */
#include <stdio.h>
#include <string.h>
#include "scram.h"

/* ---- SHA-256 (FIPS 180-4, 6.2) --------------------------------------- */

static const unsigned int K[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

#define ROR(x, n) (((x) >> (n)) | ((x) << (32 - (n))))

static void sha256_block(sha256_ctx *c, const unsigned char *b)
{
    unsigned int w[64], a, bb, cc, d, e, f, g, h;
    for (int i = 0; i < 16; i++)
        w[i] = (unsigned int)b[4 * i] << 24 | (unsigned int)b[4 * i + 1] << 16 | (unsigned int)b[4 * i + 2] << 8 | b[4 * i + 3];
    for (int i = 16; i < 64; i++) {
        unsigned int s0 = ROR(w[i - 15], 7) ^ ROR(w[i - 15], 18) ^ (w[i - 15] >> 3);
        unsigned int s1 = ROR(w[i - 2], 17) ^ ROR(w[i - 2], 19) ^ (w[i - 2] >> 10);
        w[i] = w[i - 16] + s0 + w[i - 7] + s1;
    }
    a = c->h[0]; bb = c->h[1]; cc = c->h[2]; d = c->h[3]; e = c->h[4]; f = c->h[5]; g = c->h[6]; h = c->h[7];
    for (int i = 0; i < 64; i++) {
        unsigned int t1 = h + (ROR(e, 6) ^ ROR(e, 11) ^ ROR(e, 25)) + ((e & f) ^ (~e & g)) + K[i] + w[i];
        unsigned int t2 = (ROR(a, 2) ^ ROR(a, 13) ^ ROR(a, 22)) + ((a & bb) ^ (a & cc) ^ (bb & cc));
        h = g; g = f; f = e; e = d + t1; d = cc; cc = bb; bb = a; a = t1 + t2;
    }
    c->h[0] += a; c->h[1] += bb; c->h[2] += cc; c->h[3] += d; c->h[4] += e; c->h[5] += f; c->h[6] += g; c->h[7] += h;
}

void sha256_init(sha256_ctx *c)
{
    static const unsigned int h0[8] = { 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
                                        0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 };
    memcpy(c->h, h0, sizeof h0);
    c->len = 0; c->n = 0;
}

void sha256_update(sha256_ctx *c, const void *p, size_t n)
{
    const unsigned char *s = p;
    c->len += n;
    while (n) {
        size_t k = 64 - c->n;
        if (k > n) k = n;
        memcpy(c->buf + c->n, s, k);
        c->n += (int)k; s += k; n -= k;
        if (c->n == 64) { sha256_block(c, c->buf); c->n = 0; }
    }
}

void sha256_final(sha256_ctx *c, unsigned char out[32])
{
    unsigned long long bits = c->len * 8;
    unsigned char pad = 0x80, zero = 0, lenb[8];
    sha256_update(c, &pad, 1);
    while (c->n != 56) sha256_update(c, &zero, 1);
    for (int i = 0; i < 8; i++) lenb[i] = (unsigned char)(bits >> (56 - 8 * i));
    sha256_update(c, lenb, 8);
    for (int i = 0; i < 8; i++) {
        out[4 * i] = (unsigned char)(c->h[i] >> 24); out[4 * i + 1] = (unsigned char)(c->h[i] >> 16);
        out[4 * i + 2] = (unsigned char)(c->h[i] >> 8); out[4 * i + 3] = (unsigned char)c->h[i];
    }
}

void sha256(const void *p, size_t n, unsigned char out[32])
{
    sha256_ctx c;
    sha256_init(&c); sha256_update(&c, p, n); sha256_final(&c, out);
}

/* ---- HMAC (RFC 2104) and PBKDF2 (RFC 8018, 5.2) ----------------------- */

void hmac_sha256(const void *key, size_t klen, const void *msg, size_t mlen, unsigned char out[32])
{
    unsigned char k[64], ipad[64], opad[64], inner[32];
    memset(k, 0, sizeof k);
    if (klen > 64) sha256(key, klen, k); else memcpy(k, key, klen);
    for (int i = 0; i < 64; i++) { ipad[i] = k[i] ^ 0x36; opad[i] = k[i] ^ 0x5c; }
    sha256_ctx c;
    sha256_init(&c); sha256_update(&c, ipad, 64); sha256_update(&c, msg, mlen); sha256_final(&c, inner);
    sha256_init(&c); sha256_update(&c, opad, 64); sha256_update(&c, inner, 32); sha256_final(&c, out);
}

void pbkdf2_sha256(const void *pw, size_t pwlen, const void *salt, size_t slen,
                   unsigned iterations, unsigned char *out, size_t outlen)
{
    unsigned char u[32], t[32], msg[256 + 4];
    for (unsigned block = 1; outlen; block++) {
        size_t m = slen < 256 ? slen : 256;
        memcpy(msg, salt, m);
        msg[m] = (unsigned char)(block >> 24); msg[m + 1] = (unsigned char)(block >> 16);
        msg[m + 2] = (unsigned char)(block >> 8); msg[m + 3] = (unsigned char)block;
        hmac_sha256(pw, pwlen, msg, m + 4, u);
        memcpy(t, u, 32);
        for (unsigned i = 1; i < iterations; i++) {
            hmac_sha256(pw, pwlen, u, 32, u);
            for (int j = 0; j < 32; j++) t[j] ^= u[j];
        }
        size_t k = outlen < 32 ? outlen : 32;
        memcpy(out, t, k); out += k; outlen -= k;
    }
}

/* ---- base64 (RFC 4648, 4) --------------------------------------------- */

static const char B64[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

int b64_encode(const unsigned char *p, int n, char *out, int outsz)
{
    int o = 0;
    if ((n + 2) / 3 * 4 + 1 > outsz) return -1;
    for (int i = 0; i < n; i += 3) {
        unsigned v = (unsigned)p[i] << 16 | (i + 1 < n ? (unsigned)p[i + 1] << 8 : 0) | (i + 2 < n ? p[i + 2] : 0);
        out[o++] = B64[v >> 18 & 63];
        out[o++] = B64[v >> 12 & 63];
        out[o++] = i + 1 < n ? B64[v >> 6 & 63] : '=';
        out[o++] = i + 2 < n ? B64[v & 63] : '=';
    }
    out[o] = 0;
    return o;
}

static int b64_val(int c)
{
    const char *q = c ? strchr(B64, c) : NULL;
    return q ? (int)(q - B64) : -1;
}

int b64_decode(const char *s, int n, unsigned char *out, int outsz)
{
    int o = 0;
    if (n % 4) return -1;
    for (int i = 0; i < n; i += 4) {
        int a = b64_val(s[i]), b = b64_val(s[i + 1]);
        int c = s[i + 2] == '=' ? 0 : b64_val(s[i + 2]), d = s[i + 3] == '=' ? 0 : b64_val(s[i + 3]);
        if (a < 0 || b < 0 || c < 0 || d < 0) return -1;
        if (s[i + 2] == '=' && s[i + 3] != '=') return -1;
        if ((s[i + 2] == '=' || s[i + 3] == '=') && i + 4 != n) return -1;
        unsigned v = (unsigned)a << 18 | (unsigned)b << 12 | (unsigned)c << 6 | (unsigned)d;
        if (o >= outsz) return -1;
        out[o++] = (unsigned char)(v >> 16);
        if (s[i + 2] != '=') { if (o >= outsz) return -1; out[o++] = (unsigned char)(v >> 8); }
        if (s[i + 3] != '=') { if (o >= outsz) return -1; out[o++] = (unsigned char)v; }
    }
    return o;
}

/* ---- SCRAM-SHA-256, the client (RFC 5802, 3; RFC 7677) ---------------- */

int scram_client_first(scram_state *s, const char *user, const char *cnonce, char *out, int outsz)
{
    /* the user name is sent as is: PostgreSQL ignores it (the startup
     * message named the user) and sends "n=," itself; no ',' or '=' in it */
    if (strchr(user, ',') || strchr(user, '=') || strchr(cnonce, ',')) return -1;
    int n = (int)strlen(user) + (int)strlen(cnonce) + 5;
    if (n + 1 > (int)sizeof s->client_first_bare || n + 4 > outsz) return -1;
    strcpy(s->client_first_bare, "n="); strcat(s->client_first_bare, user);
    strcat(s->client_first_bare, ",r="); strcat(s->client_first_bare, cnonce);
    strcpy(out, "n,,"); strcat(out, s->client_first_bare);
    return (int)strlen(out);
}

/* an attribute "x=" of a SCRAM message: its value and length, or NULL */
static const char *attr(const char *m, int n, char name, int *vlen)
{
    for (int i = 0; i + 1 < n; ) {
        int j = i;
        while (j < n && m[j] != ',') j++;
        if (m[i] == name && m[i + 1] == '=') { *vlen = j - i - 2; return m + i + 2; }
        i = j + 1;
    }
    return NULL;
}

int scram_client_final(scram_state *s, const char *password, const char *server_first, int sflen,
                       char *out, int outsz)
{
    int rlen, slen, ilen;
    const char *r = attr(server_first, sflen, 'r', &rlen);
    const char *sa = attr(server_first, sflen, 's', &slen);
    const char *it = attr(server_first, sflen, 'i', &ilen);
    if (!r || !sa || !it || ilen < 1 || ilen > 9) return -1;
    /* the server's nonce begins with ours (RFC 5802, 5.1) */
    const char *cn = strstr(s->client_first_bare, ",r=") + 3;
    int cnlen = (int)strlen(cn);
    if (rlen <= cnlen || memcmp(r, cn, (size_t)cnlen)) return -1;
    unsigned iter = 0;
    for (int i = 0; i < ilen; i++) {
        if (it[i] < '0' || it[i] > '9') return -1;
        iter = iter * 10 + (unsigned)(it[i] - '0');
    }
    if (!iter) return -1;
    unsigned char salt[128];
    int saltn = b64_decode(sa, slen, salt, sizeof salt);
    if (saltn < 0) return -1;

    unsigned char salted[32], ckey[32], stored[32], csig[32], proof[32], skey[32];
    pbkdf2_sha256(password, strlen(password), salt, (size_t)saltn, iter, salted, 32);
    hmac_sha256(salted, 32, "Client Key", 10, ckey);
    sha256(ckey, 32, stored);

    /* client-final-message-without-proof: the channel binding is "n,,",
     * base64 "biws" (no channel binding) */
    char without[300];
    if (rlen + 12 > (int)sizeof without) return -1;
    memcpy(without, "c=biws,r=", 9); memcpy(without + 9, r, (size_t)rlen); without[9 + rlen] = 0;
    int am = snprintf(s->auth_message, sizeof s->auth_message, "%s,%.*s,%s",
                      s->client_first_bare, sflen, server_first, without);
    if (am < 0 || am >= (int)sizeof s->auth_message) return -1;
    hmac_sha256(stored, 32, s->auth_message, (size_t)am, csig);
    for (int i = 0; i < 32; i++) proof[i] = ckey[i] ^ csig[i];
    hmac_sha256(salted, 32, "Server Key", 10, skey);
    hmac_sha256(skey, 32, s->auth_message, (size_t)am, s->server_signature);

    char p64[48];
    b64_encode(proof, 32, p64, sizeof p64);
    int n = snprintf(out, (size_t)outsz, "%s,p=%s", without, p64);
    return n < 0 || n >= outsz ? -1 : n;
}

int scram_verify_server(const scram_state *s, const char *server_final, int len)
{
    int vlen;
    const char *v = attr(server_final, len, 'v', &vlen);
    unsigned char sig[48];
    if (!v || b64_decode(v, vlen, sig, sizeof sig) != 32) return 0;
    return memcmp(sig, s->server_signature, 32) == 0;
}
