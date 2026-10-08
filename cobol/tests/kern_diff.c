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
void cob_sort_run(const unsigned char *buf, unsigned esize, unsigned klen, unsigned n, unsigned *order, unsigned *tmp);
void cob_bytes_xlat(unsigned char *p, int n, const unsigned char *tab);
void cob_bytes_sweep(unsigned char *p, int n, const unsigned char *who, const unsigned char *rep, const unsigned char *tally, int *cnt, int np);
int cob_set_decimal_point(int comma);
int cob_set_currency(int c);

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
        if (!(d->flags & COB_F_SIGNED) && rn(3) == 0) { d->flags2 |= COB_F2_NOSIGN; d->size = (unsigned)(digits + 1) / 2; }   /* COMP-6 */
        break;
    default:
        d->size = digits <= 4 ? 2 : digits <= 9 ? 4 : 8;
        if (rn(4) == 0) { d->flags |= COB_F_NOTRUNC; if (rn(3) == 0) d->size = 1 + rn(8); }
        if (rn(2)) d->flags2 |= COB_F2_BIGEND;         /* COMP's order (docs/usage.md) */
        if (!(d->flags & COB_F_SIGNED) && rn(4) == 0) d->flags2 |= COB_F2_TWOSC;                              /* a COMP-X MOVE */
        if ((d->flags & COB_F_NOTRUNC) && !(d->flags & COB_F_SIGNED) && rn(3) == 0) d->flags2 |= COB_F2_SIZEDIG;   /* 9(n) COMP-X */
        break;
    }
}

/* a random numeric-edited descriptor and its flattened PICTURE: an
 * integer part of 9, Z or * or a floating $ + - string, insertion
 * characters among them, a point and fraction digits, a fixed sign or
 * CR/DB -- the shapes picture.c accepts */
static void make_edited(cob_desc *d, char *pic)
{
    memset(d, 0, sizeof *d);
    d->cat = COB_NUM_ED;
    d->usage = COB_U_DISPLAY;
    int o = 0, digits = 0, width = 0;
    int kind = (int)rn(5);                   /* 0 9s, 1 Zs, 2 *s, 3 floating, 4 Zs then 9s */
    char fl = "$+-"[rn(3)];
    int nint = 1 + (int)rn(10), lead_sign = kind != 3 && rn(4) == 0;
    int trail = rn(4);                       /* 0 none, 1 + or -, 2 CR/DB, 3 none */
    if (lead_sign) { pic[o++] = rn(2) ? '+' : '-'; width++; d->flags |= COB_F_SIGNED; }
    if (kind == 3) { pic[o++] = fl; width++; if (fl != '$') { d->flags |= COB_F_SIGNED; trail = trail == 1 || trail == 2 ? 0 : trail; } }
    else if (rn(4) == 0 && !lead_sign) { pic[o++] = '$'; width++; }
    for (int i = 0; i < nint; i++) {
        char c = kind == 0 ? '9' : kind == 1 ? 'Z' : kind == 2 ? '*' : kind == 3 ? fl : (i < nint / 2 ? 'Z' : '9');
        pic[o++] = c; width++; digits++;
        if (i < nint - 1 && rn(4) == 0) { pic[o++] = "B0/,,"[rn(5)]; width++; }
    }
    int nfrac = rn(3) ? (int)rn(5) : 0;
    if (nfrac || rn(4) == 0) {
        pic[o++] = '.'; width++;
        for (int i = 0; i < nfrac; i++) { pic[o++] = kind == 2 && rn(2) ? '*' : kind == 1 && rn(3) == 0 ? 'Z' : '9'; width++; digits++; }
    }
    if (trail == 1 && !lead_sign && !(d->flags & COB_F_SIGNED)) { pic[o++] = rn(2) ? '+' : '-'; width++; d->flags |= COB_F_SIGNED; }
    else if (trail == 2 && !lead_sign && !(d->flags & COB_F_SIGNED)) { pic[o++] = rn(2) ? 'C' : 'D'; width += 2; d->flags |= COB_F_SIGNED; }
    pic[o] = 0;
    d->digits = (unsigned char)digits;
    d->scale = (signed char)nfrac;
    d->size = (unsigned)width;
    d->pic = pic;
    if (rn(6) == 0) d->flags |= COB_F_BLANKZ;
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
    unsigned char buf[PAD + 40 + PAD];
    char pic[48];
    int shown = 0;
    for (int it = 0; it < N; it++) {
        cob_desc d;
        int edited = rn(3) == 0;
        if (edited) {
            make_edited(&d, pic);
            cob_set_decimal_point(rn(4) == 0);             /* the locale the edited routines are handed */
            cob_set_currency(rn(4) == 0 ? "E#"[rn(2)] : '$');
        } else make_desc(&d, pic);
        for (int i = 0; i < (int)sizeof buf; i++) buf[i] = (unsigned char)rnd();
        unsigned char *p = buf + PAD;
        if (rn(5) == 0) {
            /* a fetch of whatever bytes are there: digits, spaces, overpunch, junk */
            for (unsigned i = 0; i < d.size; i++) {
                unsigned r = rn(10);
                if (edited) p[i] = r < 5 ? (unsigned char)('0' + rn(10)) : (unsigned char)" .,-+CRDB$*E#"[rn(13)];
                else p[i] = r < 6 ? (unsigned char)('0' + rn(10)) : r == 6 ? ' ' : r == 7 ? (unsigned char)('p' + rn(10)) : (unsigned char)rnd();
            }
        } else {
            long long v = make_value();
            int vscale = (int)rn(19), opts = (int)rn(4);
            int r = cob_put_num_x(p, &d, v, vscale, opts);
            mix((unsigned long long)r);
#ifdef KERN_DIFF_VERBOSE
            printf("%d put %lld %d %d pic %s -> %d\n", it, v, vscale, opts, d.pic ? d.pic : "-", r);
#endif
            if (shown < 12 && !r && (edited || shown < 6)) {
                printf("put %lld scale %d usage %d digits %d scale %d flags %d ->", v, vscale, d.usage, d.digits, d.scale, d.flags);
                if (edited) printf(" %s [%.*s]", pic, (int)d.size, (const char *)p);
                else for (unsigned i = 0; i < d.size; i++) printf(" %02x", p[i]);
                printf("\n");
                shown++;
            }
        }
        long long g = cob_get_num(p, &d);
        mix((unsigned long long)g);
#ifdef KERN_DIFF_VERBOSE
        printf("%d u%d d%d s%d f%d z%u g%lld b", it, d.usage, d.digits, d.scale, d.flags, d.size, g);
        for (int i = 0; i < (int)sizeof buf; i++) printf("%02x", buf[i]);
        printf("\n");
#endif
        for (int i = 0; i < (int)sizeof buf; i++) mix(buf[i]);
    }
    /* the SORT's run (cob_sort_run): random entries with many equal keys,
     * the order they sort into -- the hook's pointer resolution and
     * writes against the guest's own copy of the kernel */
    for (int it = 0; it < 40; it++) {
        static unsigned char ebuf[320 * 32]; static unsigned order[320], tmp[320];
        unsigned n = 1 + rn(300), klen = 1 + rn(12), esize = klen + rn(20);
        for (unsigned i = 0; i < n * esize; i++) ebuf[i] = (unsigned char)(rn(3) == 0 ? rn(256) : rn(4));
        cob_sort_run(ebuf, esize, klen, n, order, tmp);
        for (unsigned i = 0; i < n; i++) mix(order[i]);
        if (it < 3) {
            printf("sort n %u klen %u esize %u ->", n, klen, esize);
            for (unsigned i = 0; i < (n < 8 ? n : 8); i++) printf(" %u", order[i]);
            printf("\n");
        }
    }
    /* INSPECT's byte sweeps: random text, a random table, random phrases */
    for (int it = 0; it < 40; it++) {
        static unsigned char text[300], tab[256], who[256], rep[8], tally[8]; static int cnt[8];
        int n = 1 + (int)rn(300), np = 1 + (int)rn(8);
        for (int i = 0; i < n; i++) text[i] = (unsigned char)(rn(2) ? 'a' + rn(26) : rn(256));
        for (int c = 0; c < 256; c++) { tab[c] = (unsigned char)(rn(4) ? c : rn(256)); who[c] = (unsigned char)(rn(3) ? 255 : rn((unsigned)np)); }
        for (int k = 0; k < np; k++) { rep[k] = (unsigned char)rn(256); tally[k] = (unsigned char)rn(2); cnt[k] = (int)rn(1000); }
        cob_bytes_xlat(text, n, tab);
        for (int i = 0; i < n; i++) mix(text[i]);
        cob_bytes_sweep(text, n, who, rep, tally, cnt, np);
        for (int i = 0; i < n; i++) mix(text[i]);
        for (int k = 0; k < np; k++) mix((unsigned long long)cnt[k]);
        if (it < 2) { printf("sweep n %d np %d ->", n, np); for (int k = 0; k < np; k++) printf(" %d", cnt[k]); printf(" [%.8s]\n", text); }
    }
    printf("kern_diff: %d cases, hash %016llx\n", N, h);
    printf("kern_diff: done\n");
    return 0;
}
