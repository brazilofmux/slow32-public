/* test_s32utf.c -- checks common/s32utf.h on the host.
 *
 *   cc -O1 -o /tmp/test_s32utf common/test_s32utf.c && /tmp/test_s32utf [GraphemeBreakTest.txt]
 *
 * Without an argument: the decoder, the encoders and widths.  With
 * Unicode's GraphemeBreakTest.txt (www.unicode.org/Public/16.0.0/ucd/
 * auxiliary/) every line of it is run through s32u_clu_step as well;
 * lines that exercise GB9c (Indic conjunct breaks, not implemented, as
 * in libutf) are counted apart. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "s32utf.h"

static int fails;
#define CHECK(c, ...) do { if (!(c)) { fails++; printf("FAIL %s:%d: ", __FILE__, __LINE__); printf(__VA_ARGS__); printf("\n"); } } while (0)

/* decode a whole byte string with the streaming decoder */
static int decode_all(const char *s, size_t n, uint32_t *out)
{
    s32u_dec d = { 0, 0, 0, 0 };
    int k = 0;
    for (size_t i = 0; i < n; i++) {
        uint32_t cp;
        int r = s32u_dec_byte(&d, (unsigned char)s[i], &cp);
        if (r) out[k++] = cp;
        if (r == 2) i--;
    }
    uint32_t cp;
    if (s32u_dec_end(&d, &cp)) out[k++] = cp;
    return k;
}

static void dec_case(const char *name, const char *s, size_t n, const uint32_t *want, int nw)
{
    uint32_t got[64];
    int k = decode_all(s, n, got);
    int ok = k == nw;
    for (int i = 0; ok && i < k; i++) ok = got[i] == want[i];
    CHECK(ok, "decode %s: %d code points", name, k);
    /* the one-at-a-time buffer form agrees */
    int j = 0; ok = 1;
    for (size_t i = 0; i < n && ok; ) {
        uint32_t cp;
        size_t c = s32u_decode((const unsigned char *)s + i, n - i, &cp);
        ok = c >= 1 && j < nw && cp == want[j];
        i += c; j++;
    }
    CHECK(ok && j == nw, "s32u_decode %s", name);
}

static void decoder(void)
{
    uint32_t w1[] = { 0x41, 0xE9, 0x65E5, 0x1F600 };
    dec_case("valid", "A\xC3\xA9\xE6\x97\xA5\xF0\x9F\x98\x80", 10, w1, 4);
    /* the reviewer's input: C3 41 E0 80 80 ED A0 80 5A */
    uint32_t w2[] = { S32U_REPL, 0x41, S32U_REPL, S32U_REPL, S32U_REPL, S32U_REPL, S32U_REPL, S32U_REPL, 0x5A };
    dec_case("broken", "\xC3\x41\xE0\x80\x80\xED\xA0\x80\x5A", 9, w2, 9);
    uint32_t w3[] = { S32U_REPL, S32U_REPL, S32U_REPL, S32U_REPL };
    dec_case("past 10FFFF", "\xF4\x90\x80\x80", 4, w3, 4);
    uint32_t w4[] = { S32U_REPL, 0x41 };
    dec_case("truncated then A", "\xE6\x97\x41", 3, w4, 2);
    uint32_t w5[] = { S32U_REPL };
    dec_case("truncated at end", "\xF0\x9F\x98", 3, w5, 1);
    uint32_t w6[] = { S32U_REPL, S32U_REPL };
    dec_case("C0 C1", "\xC0\xC1", 2, w6, 2);
    /* Unicode 3.9 table 3-8's example: 61 F1 80 80 E1 80 C2 62 80 63 80 BF 64 */
    uint32_t w7[] = { 0x61, S32U_REPL, S32U_REPL, S32U_REPL, 0x62, S32U_REPL, 0x63, S32U_REPL, S32U_REPL, 0x64 };
    dec_case("table 3-8", "\x61\xF1\x80\x80\xE1\x80\xC2\x62\x80\x63\x80\xBF\x64", 13, w7, 10);

    unsigned char b[4];
    CHECK(s32u_encode(0xD800, b) == 3 && b[0] == 0xEF && b[1] == 0xBF && b[2] == 0xBD, "encode surrogate");
    CHECK(s32u_encode(0x1F600, b) == 4 && b[0] == 0xF0, "encode 1F600");
    unsigned char u[4]; uint32_t cp;
    CHECK(s32u_u16_put(u, 0x1F600) == 2 && s32u_u16_get(u, 2, 0, &cp) == 2 && cp == 0x1F600, "u16 pair");
    u[0] = 0xD8; u[1] = 0x00; u[2] = 0x00; u[3] = 0x41;
    CHECK(s32u_u16_get(u, 2, 0, &cp) == 1 && cp == S32U_REPL, "u16 lone high surrogate");
}

static int clu_width_of(const uint32_t *cps, int n)
{
    s32u_clu c; memset(&c, 0, sizeof c);
    for (int i = 0; i < n; i++) s32u_clu_step(&c, cps[i]);
    return s32u_clu_width(&c);
}

static void widths(void)
{
    static const struct { uint32_t cp; int w; } t[] = {
        { 'A', 1 }, { 0xE9, 1 }, { 0x65E5, 2 }, { 0x0301, 0 }, { 0x1F600, 2 }, { 0xFF21, 2 },
        { 0xAC00, 2 }, { 0x1161, 1 },   /* libutf's: a medial vowel alone; in a cluster the L's 2 wins */ { 0x3000, 2 }, { S32U_REPL, 1 }, { 0x200D, 0 },
    };
    for (size_t i = 0; i < sizeof t / sizeof *t; i++)
        CHECK(s32u_cp_width(t[i].cp) == t[i].w, "width U+%04X = %d, want %d", (unsigned)t[i].cp, s32u_cp_width(t[i].cp), t[i].w);
    uint32_t fam[] = { 0x1F468, 0x200D, 0x1F469, 0x200D, 0x1F467 };
    CHECK(clu_width_of(fam, 5) == 2, "family ZWJ sequence is one wide cluster");
    uint32_t flag[] = { 0x1F1FA, 0x1F1F8 };
    CHECK(clu_width_of(flag, 2) == 2, "flag pair is two columns");
    uint32_t heart[] = { 0x2764, 0xFE0F };
    CHECK(clu_width_of(heart, 2) == 2, "VS16 makes two");
    uint32_t eacute[] = { 'e', 0x0301 };
    CHECK(clu_width_of(eacute, 2) == 1, "e + combining acute is one");
    /* GB11: exactly one ZWJ between pictographs -- ExtPict ZWJ ZWJ ExtPict
     * is two clusters (libutf 92968c8; GraphemeBreakTest has no case) */
    {
        s32u_clu g; memset(&g, 0, sizeof g);
        s32u_clu_step(&g, 0x1F468); s32u_clu_step(&g, 0x200D); s32u_clu_step(&g, 0x200D);
        CHECK(s32u_clu_step(&g, 0x1F469) == 1, "ExtPict ZWJ ZWJ ExtPict: the second pictograph starts a cluster");
        memset(&g, 0, sizeof g);
        s32u_clu_step(&g, 0x1F468); s32u_clu_step(&g, 0x200D);
        CHECK(s32u_clu_step(&g, 0x1F469) == 0, "ExtPict ZWJ ExtPict: one cluster");
    }
    /* GB11: only an emoji joins after a ZWJ */
    s32u_clu c; memset(&c, 0, sizeof c);
    s32u_clu_step(&c, 'a'); s32u_clu_step(&c, 0x200D);
    CHECK(s32u_clu_step(&c, 'b') == 1, "a ZWJ b: b starts a cluster");
}

/* GraphemeBreakTest.txt: "÷ 0020 × 0308 ÷ ..." */
static void conformance(const char *path)
{
    FILE *f = fopen(path, "r");
    if (!f) { printf("cannot open %s\n", path); fails++; return; }
    char line[4096]; int n = 0, bad = 0, gb9c = 0;
    while (fgets(line, sizeof line, f)) {
        if (line[0] == '#' || line[0] == '\n') continue;
        char *hash = strchr(line, '#');
        int uses_9c = hash && strstr(hash, "[9.3]");
        if (hash) *hash = 0;
        uint32_t cps[64]; int brk[64], k = 0;
        char *p = line;
        int pending = 1;
        while (*p) {
            if (!strncmp(p, "\xC3\xB7", 2)) { pending = 1; p += 2; continue; }     /* ÷ */
            if (!strncmp(p, "\xC3\x97", 2)) { pending = 0; p += 2; continue; }     /* × */
            if (*p == ' ' || *p == '\t') { p++; continue; }
            char *e; unsigned long v = strtoul(p, &e, 16);
            if (e == p) { p++; continue; }
            cps[k] = (uint32_t)v; brk[k] = pending; k++; p = e;
        }
        if (!k) continue;
        n++;
        s32u_clu c; memset(&c, 0, sizeof c);
        int ok = 1;
        for (int i = 0; i < k; i++) if (s32u_clu_step(&c, cps[i]) != brk[i]) ok = 0;
        if (!ok) { if (uses_9c) gb9c++; else { bad++; if (bad <= 10) printf("GB mismatch: %s", line); } }
    }
    fclose(f);
    printf("GraphemeBreakTest: %d lines, %d disagree (GB9c, not implemented: %d)\n", n, bad, gb9c);
    if (bad) fails++;
}

int main(int argc, char **argv)
{
    decoder();
    widths();
    if (argc > 1) conformance(argv[1]);
    printf("%s\n", fails ? "FAILED" : "all passed");
    return fails != 0;
}
