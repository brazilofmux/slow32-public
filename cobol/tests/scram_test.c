/* scram_test.c -- libcob/scram.c against the published vectors: SHA-256
 * (FIPS 180-4 examples), HMAC-SHA-256 (RFC 4231 cases 1, 2, 6),
 * PBKDF2-HMAC-SHA-256 (RFC 7914, 11; and the common c=1/4096 values), and
 * a whole SCRAM-SHA-256 exchange (RFC 7677, 3); and libpq's password-file
 * rules (pgwire.c's pg_password_from_file).  Built for the host by
 * run-tests.sh; prints "scram_test: N vectors" and exits 0 when all hold. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "../libcob/scram.c"
#include "../libcob/pgwire.c"

static int fails, count;

static void hex(const unsigned char *p, int n, char *out)
{
    for (int i = 0; i < n; i++) sprintf(out + 2 * i, "%02x", p[i]);
}
static void check(const char *what, const unsigned char *got, int n, const char *want)
{
    char h[300];
    hex(got, n, h);
    count++;
    if (strcmp(h, want)) { fails++; printf("FAIL %s\n  got  %s\n  want %s\n", what, h, want); }
}
static void check_s(const char *what, const char *got, const char *want)
{
    count++;
    if (strcmp(got, want)) { fails++; printf("FAIL %s\n  got  %s\n  want %s\n", what, got, want); }
}

int main(void)
{
    unsigned char d[64];
    sha256("abc", 3, d);
    check("sha256 abc", d, 32, "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
    sha256("", 0, d);
    check("sha256 empty", d, 32, "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
    const char *m2 = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq";
    sha256(m2, strlen(m2), d);
    check("sha256 448 bits", d, 32, "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1");
    {
        sha256_ctx c; sha256_init(&c);
        char a[1000]; memset(a, 'a', sizeof a);
        for (int i = 0; i < 1000; i++) sha256_update(&c, a, sizeof a);
        sha256_final(&c, d);
        check("sha256 million a", d, 32, "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0");
    }

    unsigned char k[131];
    memset(k, 0x0b, 20);
    hmac_sha256(k, 20, "Hi There", 8, d);
    check("hmac rfc4231 1", d, 32, "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7");
    hmac_sha256("Jefe", 4, "what do ya want for nothing?", 28, d);
    check("hmac rfc4231 2", d, 32, "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843");
    memset(k, 0xaa, 131);
    const char *m6 = "Test Using Larger Than Block-Size Key - Hash Key First";
    hmac_sha256(k, 131, m6, strlen(m6), d);
    check("hmac rfc4231 6", d, 32, "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54");

    pbkdf2_sha256("passwd", 6, "salt", 4, 1, d, 64);
    check("pbkdf2 rfc7914 1", d, 64, "55ac046e56e3089fec1691c22544b605f94185216dde0465e68b9d57c20dacbc"
                                     "49ca9cccf179b645991664b39d77ef317c71b845b1e30bd509112041d3a19783");
    pbkdf2_sha256("password", 8, "salt", 4, 1, d, 32);
    check("pbkdf2 c=1", d, 32, "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b");
    pbkdf2_sha256("password", 8, "salt", 4, 4096, d, 32);
    check("pbkdf2 c=4096", d, 32, "c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a");

    char b[64];
    b64_encode((const unsigned char *)"n,,", 3, b, sizeof b); check_s("b64 n,,", b, "biws");
    b64_encode((const unsigned char *)"fo", 2, b, sizeof b); check_s("b64 fo", b, "Zm8=");
    b64_encode((const unsigned char *)"f", 1, b, sizeof b); check_s("b64 f", b, "Zg==");
    int n = b64_decode("Zm9vYmFy", 8, d, sizeof d); d[n < 0 ? 0 : n] = 0; check_s("b64 decode", (char *)d, "foobar");
    count++; if (b64_decode("Zm9=Ym", 6, d, sizeof d) != -1) { fails++; printf("FAIL b64 malformed accepted\n"); }

    /* RFC 7677, 3: user "user", password "pencil" */
    scram_state s;
    char out[512];
    scram_client_first(&s, "user", "rOprNGfwEbeRWgbNEkqO", out, sizeof out);
    check_s("scram client-first", out, "n,,n=user,r=rOprNGfwEbeRWgbNEkqO");
    const char *sf = "r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096";
    scram_client_final(&s, "pencil", sf, (int)strlen(sf), out, sizeof out);
    check_s("scram client-final", out,
            "c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ=");
    const char *svf = "v=6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4=";
    count++; if (!scram_verify_server(&s, svf, (int)strlen(svf))) { fails++; printf("FAIL scram server signature\n"); }
    count++; if (scram_verify_server(&s, "v=AAAA", 6)) { fails++; printf("FAIL a wrong server signature accepted\n"); }
    const char *bad = "r=SOMEONEELSE%x,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096";
    count++; if (scram_client_final(&s, "pencil", bad, (int)strlen(bad), out, sizeof out) != -1) { fails++; printf("FAIL a foreign nonce accepted\n"); }

    /* the password file, as libpq reads it */
    char path[] = "/tmp/scram_test_pgpass_XXXXXX";
    int fd = mkstemp(path);
    FILE *pf = fdopen(fd, "w");
    fputs("# a comment\n"
          "\n"
          "dbhost:5432:ledgerdb:user:pw1:the rest after a colon\n"
          "dbhost:5432:ledgerdb:user:second-match\n"
          "other:5432:*:user:pw2\n"
          "*:*:otherdb:*:pw3\r\n"
          "h\\:x:5432:db:u:p\\:q\\\\r\n", pf);
    fclose(pf);
    chmod(path, 0600);
    struct { const char *h, *p, *d, *u, *want; } pc[] = {
        { "dbhost", "5432", "ledgerdb", "user", "pw1" },               /* to the first ':'; first line wins */
        { "other", "5432", "anything", "user", "pw2" },                /* '*' */
        { "z", "1", "otherdb", "bob", "pw3" },                         /* CR LF line end */
        { "h:x", "5432", "db", "u", "p:q\\r" },                        /* escaped ':' and '\' */
        { "dbhost", "5433", "ledgerdb", "user", NULL },                /* no match */
    };
    for (int i = 0; i < (int)(sizeof pc / sizeof pc[0]); i++) {
        char pw[64];
        int got = pg_password_from_file(path, pc[i].h, pc[i].p, pc[i].d, pc[i].u, pw, sizeof pw);
        count++;
        if (pc[i].want ? !got || strcmp(pw, pc[i].want) : got) {
            fails++; printf("FAIL pgpass %s:%s:%s:%s got %s want %s\n", pc[i].h, pc[i].p, pc[i].d, pc[i].u,
                            got ? pw : "(none)", pc[i].want ? pc[i].want : "(none)");
        }
    }
    chmod(path, 0644);                  /* readable by others: libpq ignores it */
    { char pw[64]; count++; if (pg_password_from_file(path, "dbhost", "5432", "ledgerdb", "user", pw, sizeof pw)) { fails++; printf("FAIL pgpass: a 0644 file was used\n"); } }
    unlink(path);

    printf("scram_test: %d vectors%s\n", count, fails ? ", FAILURES" : "");
    return fails != 0;
}
