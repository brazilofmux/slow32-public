/* A memcpy of a constant size through pointers, and a struct
 * assignment of up to 64 bytes, copy inline as words with a byte
 * tail; larger ones call memcpy.  SQLite's sqlite3VdbeMemShallowCopy
 * copies MEMCELLSIZE bytes a million times in the 200,000-row run and
 * sqlite3IsNaN eight bytes into a double; each was a call.  The
 * harness counts the calls left in this file: exactly one, big(). */
#include <stdio.h>
#include <string.h>
typedef unsigned long long u64;
struct cell { int a; char tag[13]; int b; double d; u64 w; };   /* 40 bytes */
struct wide { char name[31]; int n[8]; };                        /* 63 bytes */
struct huge { int v[25]; };                                      /* 100 bytes */

static void shallow(struct cell *to, const struct cell *from) {
    memcpy(to, from, 24);                 /* a, tag, b, part of d: constant through pointers */
}
static int is_nan(double x) {
    u64 y;
    memcpy(&y, &x, sizeof(y));
    return ((y >> 52) & 0x7ff) == 0x7ff && (y & 0xfffffffffffffULL) != 0;
}
static void unaligned(char *dst, const char *src) {
    memcpy(dst + 3, src + 1, 13);         /* neither end word-aligned */
}
static struct wide wcopy(const struct wide *w) {
    struct wide r;
    r = *w;                               /* 63 bytes: inline now */
    r.n[7] = r.n[7] + 1;
    return r;
}
static void big(struct huge *d, const struct huge *s) {
    *d = *s;                              /* 100 bytes: still a call */
}

int main(void) {
    struct cell c1, c2;
    struct wide w1, w2;
    struct huge h1, h2;
    char a[20], b[20];
    int i;
    double zero = 0.0;
    memset(&c1, 0, sizeof c1); memset(&c2, 0x5a, sizeof c2);
    c1.a = 7; strcpy(c1.tag, "twelve chars"); c1.b = -9; c1.d = 2.5; c1.w = 0x1122334455667788ULL;
    shallow(&c2, &c1);
    if (c2.a != 7 || strcmp(c2.tag, "twelve chars") != 0 || c2.b != -9) return 1;
    if (c2.w != 0x5a5a5a5a5a5a5a5aULL) return 2;          /* untouched past 24 */
    if (!is_nan(zero / zero) || is_nan(1.5) || is_nan(-0.0)) return 3;
    for (i = 0; i < 20; i++) { a[i] = (char)('a' + i); b[i] = '.'; }
    unaligned(b, a);
    if (b[2] != '.' || b[3] != 'b' || b[15] != 'n' || b[16] != '.') return 4;
    memset(&w1, 0, sizeof w1); strcpy(w1.name, "thirty characters of name here"); for (i = 0; i < 8; i++) w1.n[i] = i * i;
    w2 = wcopy(&w1);
    if (strcmp(w2.name, w1.name) != 0 || w2.n[6] != 36 || w2.n[7] != 50) return 5;
    for (i = 0; i < 25; i++) h1.v[i] = 1000 - i;
    big(&h2, &h1);
    if (h2.v[0] != 1000 || h2.v[24] != 976) return 6;
    printf("memcpy inline ok\n");
    return 0;
}
