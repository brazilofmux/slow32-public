/* kern_diff.c -- the hook differential for libcob/kern.h (docs/dbt-hooks.md).
 *
 * A guest program: it drives cob_put_num_x and cob_get_num, the hookable
 * routines, over random descriptors (every usage, sign and flag the
 * kernels take, P-scaled pictures, ROUNDED and size-error options),
 * random values and scales, and random bytes to fetch, and prints a hash
 * of every result and every byte around each item.  Run it under
 * slow32-fast (no hooks) and slow32-dbt (hooks) and the two outputs must
 * be identical; tests/kern-differential.sh does that, and checks that
 * the hooks were called and the sentinel printed. */
#include <stdio.h>
#include <string.h>
#include "cobrt.h"

long long cob_get_num(const void *p, const cob_desc *d);
int cob_put_num_x(void *p, const cob_desc *d, long long v, int vscale, int opts);

static unsigned long long rs = 88172645463325252ULL;
static unsigned long long rnd(void) { rs ^= rs << 13; rs ^= rs >> 7; rs ^= rs << 17; return rs; }
static unsigned rn(unsigned n) { return (unsigned)(rnd() % n); }

static unsigned long long h = 1469598103934665603ULL;
static void mix(unsigned long long v) { h ^= v; h *= 1099511628211ULL; }

/* a random descriptor the kernels take, and its PICTURE (P symbols) */
static void make_desc(cob_desc *d, char *pic)
{
    memset(d, 0, sizeof *d);
    d->cat = COB_NUM;
    d->usage = (unsigned char)(rn(3) == 0 ? COB_U_BINARY : rn(2) ? COB_U_DISPLAY : COB_U_PACKED);
    int digits = 1 + (int)rn(18);
    d->digits = (unsigned char)digits;
    int np = rn(4) == 0 ? (int)rn(digits < 4 ? digits : 4) : 0;      /* P positions */
    int eff = digits - np;
    if (np) {                                /* PPP999 (scale past the digits) or 999PPP (negative) */
        int lead = rn(2);
        int o = 0;
        if (lead) { for (int i = 0; i < np; i++) pic[o++] = 'P'; for (int i = 0; i < eff; i++) pic[o++] = '9'; d->scale = (signed char)digits; }
        else { for (int i = 0; i < eff; i++) pic[o++] = '9'; for (int i = 0; i < np; i++) pic[o++] = 'P'; d->scale = (signed char)-np; }
        pic[o] = 0;
        d->pic = pic;
    } else {
        d->scale = (signed char)(rn(3) ? (int)rn((unsigned)digits + 1) : 0);
        d->pic = rn(2) ? 0 : "999";          /* a PICTURE with no P: the kernel only counts P */
    }
    if (rn(2)) d->flags |= COB_F_SIGNED;
    switch (d->usage) {
    case COB_U_DISPLAY:
        if ((d->flags & COB_F_SIGNED) && rn(3) == 0) d->flags |= rn(2) ? COB_F_SEPLEAD : COB_F_SEPTRAIL;
        else if ((d->flags & COB_F_SIGNED) && rn(4) == 0) d->flags |= COB_F_LEAD;
        if (rn(8) == 0) d->flags |= COB_F_BLANKZ;
        d->size = (unsigned)eff + ((d->flags & (COB_F_SEPLEAD | COB_F_SEPTRAIL)) ? 1u : 0u);
        break;
    case COB_U_PACKED:
        d->size = (unsigned)digits / 2 + 1;
        break;
    default:
        d->size = digits <= 4 ? 2 : digits <= 9 ? 4 : 8;
        if (rn(4) == 0) { d->flags |= COB_F_NOTRUNC; if (rn(3) == 0) d->size = 1 + rn(8); }
        break;
    }
}

/* a random value: any magnitude up to 19 digits, and sometimes an edge */
static long long make_value(void)
{
    static const long long edge[] = { 0, 1, -1, 9, 10, 99, 100, 999999999, 1000000000, -1000000000,
                                      999999999999999999LL, -999999999999999999LL, 1000000000000000000LL,
                                      0x7FFFFFFFFFFFFFFFLL, (-0x7FFFFFFFFFFFFFFFLL - 1), 5, -5, 50, -50, 500 };
    if (rn(6) == 0) return edge[rn(sizeof edge / sizeof edge[0])];
    unsigned long long m = 1; int k = (int)rn(20);
    for (int i = 0; i < k; i++) m *= 10;
    long long v = (long long)(rnd() % m);
    return rn(2) ? -v : v;
}

int main(void)
{
    enum { N = 300000, PAD = 4 };
    unsigned char buf[PAD + 16 + PAD];
    char pic[24];
    int shown = 0;
    for (int it = 0; it < N; it++) {
        cob_desc d;
        make_desc(&d, pic);
        for (int i = 0; i < (int)sizeof buf; i++) buf[i] = (unsigned char)rnd();
        unsigned char *p = buf + PAD;
        if (rn(5) == 0) {
            /* a fetch of whatever bytes are there: digits, spaces, overpunch, junk */
            for (unsigned i = 0; i < d.size; i++) {
                unsigned r = rn(10);
                p[i] = r < 6 ? (unsigned char)('0' + rn(10)) : r == 6 ? ' ' : r == 7 ? (unsigned char)('p' + rn(10)) : (unsigned char)rnd();
            }
        } else {
            long long v = make_value();
            int vscale = (int)rn(19), opts = (int)rn(4);
            int r = cob_put_num_x(p, &d, v, vscale, opts);
            mix((unsigned long long)r);
            if (shown < 6 && !r) {
                printf("put %lld scale %d usage %d digits %d scale %d flags %d ->", v, vscale, d.usage, d.digits, d.scale, d.flags);
                for (unsigned i = 0; i < d.size; i++) printf(" %02x", p[i]);
                printf("\n");
                shown++;
            }
        }
        long long g = cob_get_num(p, &d);
        mix((unsigned long long)g);
        for (int i = 0; i < (int)sizeof buf; i++) mix(buf[i]);
    }
    printf("kern_diff: %d cases, hash %016llx\n", N, h);
    printf("kern_diff: done\n");
    return 0;
}
