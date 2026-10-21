/* The "crypto" service through <openssl/evp.h>'s shape: every digest the
 * host offers, incrementally and at once, HMAC with short, empty and long
 * keys, a context used again after its final, lookups by name, and random
 * bytes.  The expected output was computed apart from this code, by
 * Python's hashlib and hmac; the host's platform library does the work
 * here, so this holds the service to an independent answer. */
#include <stdio.h>
#include <string.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <openssl/md5.h>
#include <openssl/rand.h>
#include <openssl/sha.h>

static void hex(const char *what, const unsigned char *p, unsigned n) {
    printf("%-22s ", what);
    for (unsigned i = 0; i < n; i++) printf("%02x", p[i]);
    printf("\n");
}

static unsigned char big[100000];

int main(void) {
    static const char *names[] = { "md5", "sha1", "sha224", "sha256", "sha384", "sha512" };
    static const char sentence[] = "Pack my box with five dozen liquor jugs";
    unsigned char out[EVP_MAX_MD_SIZE];
    unsigned int n = 0;

    for (int i = 0; i < 6; i++) {
        const EVP_MD *md = EVP_get_digestbyname(names[i]);
        char label[32];
        EVP_Digest("", 0, out, &n, md, NULL);
        snprintf(label, sizeof label, "%s(empty)", names[i]);
        hex(label, out, n);
        EVP_Digest(sentence, strlen(sentence), out, &n, md, NULL);
        snprintf(label, sizeof label, "%s(sentence)", names[i]);
        hex(label, out, n);
    }

    /* 100000 bytes, in pieces that do not line up with anything, against
     * the same bytes at once; and the context used again */
    for (unsigned i = 0; i < sizeof big; i++) big[i] = (unsigned char)(i * 7 + 3);
    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    EVP_DigestInit_ex(ctx, EVP_sha256(), NULL);
    size_t at = 0, step = 1;
    while (at < sizeof big) {
        size_t k = step > sizeof big - at ? sizeof big - at : step;
        EVP_DigestUpdate(ctx, big + at, k);
        at += k;
        step = step * 3 + 1;
    }
    unsigned char inc[EVP_MAX_MD_SIZE], once[EVP_MAX_MD_SIZE];
    unsigned int ni = 0, no = 0;
    EVP_DigestFinal_ex(ctx, inc, &ni);
    EVP_Digest(big, sizeof big, once, &no, EVP_sha256(), NULL);
    hex("sha256(big)", once, no);
    printf("pieces agree %d\n", ni == no && memcmp(inc, once, no) == 0);
    printf("update after final refused %d\n", EVP_DigestUpdate(ctx, "x", 1) == 0);
    EVP_DigestInit_ex(ctx, NULL, NULL);
    EVP_DigestUpdate(ctx, "abc", 3);
    EVP_DigestFinal_ex(ctx, out, &n);
    hex("reused sha256(abc)", out, n);
    EVP_DigestInit(ctx, EVP_sha512());
    EVP_DigestUpdate(ctx, big, sizeof big);
    EVP_DigestFinal(ctx, out, &n);
    hex("sha512(big)", out, n);
    EVP_MD_CTX_free(ctx);

    unsigned char key20[20];
    memset(key20, 0x0b, sizeof key20);
    hex("hmac-sha256 key20", HMAC(EVP_sha256(), key20, 20, (const unsigned char *)"Hi There", 8, out, &n), n);
    hex("hmac-sha1 empty key", HMAC(EVP_sha1(), "", 0, (const unsigned char *)sentence, strlen(sentence), out, &n), n);
    unsigned char key150[150];
    memset(key150, 0xaa, sizeof key150);
    hex("hmac-sha384 key150", HMAC(EVP_sha384(), key150, 150, (const unsigned char *)sentence, strlen(sentence), out, &n), n);
    hex("hmac-md5 key4", HMAC(EVP_md5(), "lamp", 4, big, sizeof big, out, &n), n);
    hex("hmac-sha512 key4", HMAC(EVP_sha512(), "lamp", 4, (const unsigned char *)"", 0, NULL, &n), n);

    hex("SHA1()", SHA1((const unsigned char *)"abc", 3, NULL), SHA_DIGEST_LENGTH);
    hex("SHA224()", SHA224((const unsigned char *)"abc", 3, NULL), SHA224_DIGEST_LENGTH);
    hex("MD5()", MD5((const unsigned char *)"abc", 3, out), MD5_DIGEST_LENGTH);

    printf("names: SHA-256 %d, SHA2-512 %d, Sha1 %d, whirlpool %d\n",
           EVP_get_digestbyname("SHA-256") == EVP_sha256(), EVP_get_digestbyname("SHA2-512") == EVP_sha512(),
           EVP_get_digestbyname("Sha1") == EVP_sha1(), EVP_get_digestbyname("whirlpool") == NULL);
    printf("sizes: %d %d %d, blocks %d %d\n", EVP_MD_size(EVP_md5()), EVP_MD_size(EVP_sha384()),
           EVP_MD_get_size(EVP_sha512()), EVP_MD_block_size(EVP_sha256()), EVP_MD_block_size(EVP_sha512()));
    printf("max block %d\n", EVP_MAX_BLOCK_LENGTH);

    unsigned char r1[64], r2[64];
    int ok = RAND_bytes(r1, sizeof r1) && RAND_bytes(r2, sizeof r2);
    printf("random: %d, two draws differ %d, status %d\n", ok, memcmp(r1, r2, sizeof r1) != 0, RAND_status());
    return 0;
}
