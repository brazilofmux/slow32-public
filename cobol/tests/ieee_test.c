/* ieee_test.c -- libcob/ieee.h against tests/ieee_vectors.py's exact
 * arithmetic, on the host:
 *     cc -O1 -w -o ieee_test tests/ieee_test.c && python3 tests/ieee_vectors.py | ./ieee_test
 * Every vector encodes and decodes both ways (a decimal that the format
 * holds exactly must come back as itself; a binary128 must decode to the
 * 36 digits the script computed, and that decoding must encode back to
 * the same bits). */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../libcob/wide.h"
#include "../libcob/ieee.h"

static void hex_to_bytes(const char *h, unsigned char *b, int n)
{
    for (int i = 0; i < n; i++) { unsigned v; sscanf(h + 2 * i, "%2x", &v); b[i] = (unsigned char)v; }
}
static void bytes_to_hex(const unsigned char *b, int n, char *h)
{
    for (int i = 0; i < n; i++) sprintf(h + 2 * i, "%02x", b[i]);
}
/* the magnitude's digits without leading zeros */
static void mag_str(const wl_t *mag, char *out)
{
    char d[40]; int n = w_to_digits(mag, d, 39);
    if (!n) { strcpy(out, "0"); return; }
    memcpy(out, d + 39 - n, (size_t)n); out[n] = 0;
}
/* a decimal with trailing zeros and a scale normalized: 1200 scale 2 == 12 scale 0 */
static void normalize(wl_t *mag, int *scale)
{
    while (!mp_is_zero(mag, WL)) {
        wl_t t[WL]; memcpy(t, mag, sizeof t);
        if (mp_div_small(t, WL, 10)) break;
        memcpy(mag, t, sizeof t); (*scale)--;
    }
    if (mp_is_zero(mag, WL)) *scale = 0;
}

int main(void)
{
    char line[8192]; int n = 0, bad = 0;
    while (fgets(line, sizeof line, stdin)) {
        char kind[16], digs[8192], hex[80]; int scale, neg;
        n++;
        if (!strncmp(line, "x128 ", 5)) {
            if (sscanf(line, "%15s %79s %8191s %d", kind, hex, digs, &scale) != 4) { printf("bad line %d\n", n); bad++; continue; }
            unsigned char b[16]; hex_to_bytes(hex, b, 16);
            wl_t mag[WL]; int sc, ng;
            int r = bin128_decode(b, 1, mag, &sc, &ng);
            char got[40]; mag_str(mag, got);
            wl_t want[WL]; w_from_digits(want, digs, (int)strlen(digs));
            normalize(mag, &sc); int ws = scale; normalize(want, &ws);
            if (r != 0 || mp_cmp(mag, want, WL) || sc != ws) { printf("x128 %s: want %s e%d got %s e%d (r=%d)\n", hex, digs, -scale, got, -sc, r); bad++; continue; }
            /* and back */
            unsigned char c[16]; w_from_digits(want, digs, (int)strlen(digs));
            int neg2 = ng;
            if (bin128_encode(c, 1, want, scale, neg2)) { printf("x128 %s: re-encode overflowed\n", hex); bad++; continue; }
            char h2[40]; bytes_to_hex(c, 16, h2);
            if (strcmp(h2, hex)) {
                /* 36 digits may round-trip to a neighbour only when the decoding was not exact: allow one ulp? no: 36 digits identify a binary128 */
                printf("x128 %s: re-encoded as %s\n", hex, h2); bad++;
            }
            continue;
        }
        if (sscanf(line, "%15s %8191s %d %d %79s", kind, digs, &scale, &neg, hex) != 5) { printf("bad line %d\n", n); bad++; continue; }
        wl_t mag[WL]; w_from_digits(mag, digs, (int)strlen(digs));
        if (!strcmp(kind, "b128")) {
            unsigned char b[16]; int r = bin128_encode(b, 1, mag, scale, neg);
            char h[40]; bytes_to_hex(b, 16, h);
            if (!strcmp(hex, "overflow")) { if (!r) { printf("b128 %s e%d: want overflow, got %s\n", digs, -scale, h); bad++; } continue; }
            if (r || strcmp(h, hex)) { printf("b128 %s e%d: want %s got %s (r=%d)\n", digs, -scale, hex, h, r); bad++; }
            continue;
        }
        int size = kind[1] == '6' ? 8 : 16, dpd = kind[strlen(kind) - 1] == 'd';
        unsigned char b[16]; int r = dec_encode(b, size, 1, dpd, mag, scale, neg);
        char h[40]; bytes_to_hex(b, size, h);
        if (r || strcmp(h, hex)) { printf("%s %s e%d: want %s got %s (r=%d)\n", kind, digs, -scale, hex, h, r); bad++; continue; }
        wl_t m2[WL]; int s2, n2;
        unsigned char c[16]; hex_to_bytes(hex, c, size);
        r = dec_decode(c, size, 1, dpd, m2, &s2, &n2);
        /* the other byte order, from the same value */
        unsigned char le[16]; dec_encode(le, size, 0, dpd, mag, scale, neg);
        for (int i = 0; i < size; i++) if (le[i] != c[size - 1 - i]) { printf("%s %s: byte order\n", kind, hex); bad++; break; }
        if (mp_is_zero(mag, WL) && (scale != s2)) { printf("%s decode %s: zero's exponent %d came back as %d\n", kind, hex, -scale, -s2); bad++; }
        normalize(mag, &scale); normalize(m2, &s2);
        char got[40]; mag_str(m2, got);
        if (r || mp_cmp(mag, m2, WL) || scale != s2 || (neg != n2 && !mp_is_zero(mag, WL))) { printf("%s decode %s: want %s e%d got %s e%d\n", kind, hex, digs, -scale, got, -s2); bad++; }
    }
    printf("ieee: %d vectors, %d wrong\n", n, bad);
    return bad != 0;
}
