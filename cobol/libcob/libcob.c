/* libcob -- the SLOW-32 COBOL runtime.
 *
 * Guest code, built by the SLOW-32 C toolchain (cobol/ is in the tree's
 * ordinary universe, so the host compiles it).  The compiler lowers each
 * verb either to a short inline sequence or to a call in here with a
 * descriptor it built (cobrt.h); the runtime works in bytes and pictures
 * and knows nothing about the statement that called it.
 *
 * Stage 2: DISPLAY; MOVE across the conversion matrix (unedited cells);
 * comparison; class tests; a scaled-i64 numeric stack for the arithmetic
 * statements; the PERFORM stack.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cobrt.h"

/* ---- output: DISPLAY goes to stdout, buffered by us ------------------ */

static char out_buf[512];
static int  out_n;

/* Through stdio, not write(2): the DEBUG-instruction libc has no write,
 * and fwrite is byte-safe (COBOL data may hold NULs) under both libcs. */
static void out_flush(void)
{
    if (out_n) { fwrite(out_buf, 1, out_n, stdout); fflush(stdout); out_n = 0; }
}

static void out_bytes(const char *p, int n)
{
    while (n > 0) {
        int room = (int)sizeof out_buf - out_n;
        int k = n < room ? n : room;
        memcpy(out_buf + out_n, p, k);
        out_n += k; p += k; n -= k;
        if (out_n == (int)sizeof out_buf) out_flush();
    }
}

static void out_char(char c) { out_bytes(&c, 1); }

static void cob_fatal(const char *msg)
{
    out_flush();
    fputs("libcob: ", stderr); fputs(msg, stderr); fputc('\n', stderr);
    exit(3);
}

/* ---- program lifetime -------------------------------------------------- */

void cob_init(void)
{
    out_n = 0;
}

void cob_stop_run(int code)
{
    out_flush();
    exit(code);
}

/* ---- numeric access: the canonical numeric is a scaled i64 ------------ */

static const long long pow10tab[19] = {
    1LL, 10LL, 100LL, 1000LL, 10000LL, 100000LL, 1000000LL, 10000000LL,
    100000000LL, 1000000000LL, 10000000000LL, 100000000000LL,
    1000000000000LL, 10000000000000LL, 100000000000000LL,
    1000000000000000LL, 10000000000000000LL, 100000000000000000LL,
    1000000000000000000LL };

static int capacity_digits(unsigned size)
{
    return size == 1 ? 3 : size == 2 ? 5 : size == 4 ? 10 : 19;
}

/* Value of the item scaled by 10^scale (i.e. the integer the digits spell). */
long long cob_get_num(const void *vp, const cob_desc *d)
{
    const unsigned char *p = vp;
    long long v = 0;
    int neg = 0;

    switch (d->usage) {
    case COB_U_BINARY: {
        unsigned long long u = 0;
        for (int i = (int)d->size - 1; i >= 0; i--) u = (u << 8) | p[i];
        if ((d->flags & COB_F_SIGNED) && d->size < 8 && ((u >> (d->size * 8 - 1)) & 1))
            u |= ~0ULL << (d->size * 8);
        return (long long)u;
    }
    case COB_U_PACKED: {
        int bytes = (int)d->size;
        for (int i = 0; i < bytes; i++) {
            v = v * 10 + (p[i] >> 4);
            if (i < bytes - 1) v = v * 10 + (p[i] & 15);
        }
        if ((p[bytes - 1] & 15) == 0xD) v = -v;
        return v;
    }
    default: {
        /* DISPLAY: digits, with the sign either overpunched on the last
         * digit ('p'..'y' = negative 0..9), or separate leading/trailing */
        int n = (int)d->size, i = 0;
        if (d->flags & COB_F_SEPLEAD) { neg = (p[0] == '-'); i = 1; }
        int end = n;
        if (d->flags & COB_F_SEPTRAIL) { neg = (p[n - 1] == '-'); end = n - 1; }
        for (; i < end; i++) {
            unsigned char c = p[i];
            if (c >= 'p' && c <= 'y') { v = v * 10 + (c - 'p'); neg = 1; }
            else if (c >= '0' && c <= '9') v = v * 10 + (c - '0');
            else if (c == ' ') v = v * 10;             /* a space counts as zero */
            else v = v * 10 + (c & 15);                 /* GnuCOBOL: low nibble */
        }
        return neg ? -v : v;
    }
    }
}

/* Store v (scaled by vscale) into the item, aligning the scale by
 * truncation and truncating high-order digits to the picture (unless the
 * usage says the binary field's capacity is the limit). */
void cob_put_num(void *vp, const cob_desc *d, long long v, int vscale)
{
    unsigned char *p = vp;
    if (vscale > d->scale) v /= pow10tab[vscale - d->scale];
    else if (vscale < d->scale) v *= pow10tab[d->scale - vscale];

    int neg = v < 0;
    unsigned long long mag = neg ? (unsigned long long)(-v) : (unsigned long long)v;
    if (!(d->flags & COB_F_NOTRUNC) && d->digits <= 18)
        mag %= (unsigned long long)pow10tab[d->digits];
    if (!(d->flags & COB_F_SIGNED)) neg = 0;         /* unsigned takes the magnitude */

    switch (d->usage) {
    case COB_U_BINARY: {
        long long s = neg ? -(long long)mag : (long long)mag;
        for (unsigned i = 0; i < d->size; i++) p[i] = (unsigned char)(s >> (8 * i));
        break;
    }
    case COB_U_PACKED: {
        int digits = d->digits, bytes = (int)d->size;
        char dg[20];
        for (int i = digits - 1; i >= 0; i--) { dg[i] = (char)(mag % 10); mag /= 10; }
        memset(p, 0, bytes);
        int nib = bytes * 2 - 2;             /* nibble index of the last digit */
        for (int i = digits - 1; i >= 0; i--, nib--) {
            if (nib & 1) p[nib / 2] |= (unsigned char)dg[i];
            else p[nib / 2] |= (unsigned char)(dg[i] << 4);
        }
        p[bytes - 1] |= (d->flags & COB_F_SIGNED) ? (neg ? 0xD : 0xC) : 0xF;
        break;
    }
    default: {
        int n = (int)d->size, i = n - 1, start = 0;
        if (d->flags & COB_F_SEPLEAD) { p[0] = neg ? '-' : '+'; start = 1; }
        if (d->flags & COB_F_SEPTRAIL) { p[n - 1] = neg ? '-' : '+'; i = n - 2; }
        for (; i >= start; i--) { p[i] = (unsigned char)('0' + mag % 10); mag /= 10; }
        if (neg && !(d->flags & (COB_F_SEPLEAD | COB_F_SEPTRAIL)))
            p[n - 1] = (unsigned char)(p[n - 1] - '0' + 'p');
        break;
    }
    }
}

/* ---- DISPLAY ---------------------------------------------------------- */

void cob_display(const char *p, int n) { out_bytes(p, n); }
void cob_display_nl(void) { out_char('\n'); out_flush(); }
void cob_display_flush(void) { out_flush(); }

/* Emit a magnitude as `digits` characters with a point where the scale
 * says, a leading sign when the picture is signed.  GnuCOBOL's convention;
 * the standard leaves DISPLAY of numeric items to the implementor. */
static void emit_scaled(unsigned long long mag, int neg, int digits, int scale, int is_signed)
{
    char d[40];
    int n = 0;
    if (is_signed) out_char(neg ? '-' : '+');
    for (int i = 0; i < digits; i++) { d[n++] = (char)('0' + mag % 10); mag /= 10; }
    for (int i = n - 1; i >= 0; i--) {
        if (scale > 0 && i == scale - 1) out_char('.');
        out_char(d[i]);
    }
}

void cob_display_field(const void *vp, const cob_desc *d)
{
    const unsigned char *p = vp;
    if (d->cat != COB_NUM) { out_bytes((const char *)p, (int)d->size); return; }
    if (d->usage == COB_U_DISPLAY && !(d->flags & (COB_F_SEPLEAD | COB_F_SEPTRAIL))) {
        int n = (int)d->size;
        unsigned char last = p[n - 1];
        int neg = (d->flags & COB_F_SIGNED) && last >= 'p' && last <= 'y';
        if (d->flags & COB_F_SIGNED) out_char(neg ? '-' : '+');
        for (int i = 0; i < n; i++) {
            if (d->scale > 0 && i == n - d->scale) out_char('.');
            unsigned char c = p[i];
            if (i == n - 1 && neg) c = (unsigned char)(last - 'p' + '0');
            out_char((char)c);
        }
        return;
    }
    long long v = cob_get_num(p, d);
    int neg = v < 0;
    unsigned long long mag = neg ? (unsigned long long)(-v) : (unsigned long long)v;
    int digits = (d->flags & COB_F_NOTRUNC) ? capacity_digits(d->size) : d->digits;
    emit_scaled(mag, neg, digits, d->scale, d->flags & COB_F_SIGNED);
}

/* ---- MOVE ------------------------------------------------------------- */

/* alphanumeric to alphanumeric: left-justified, space-filled, truncated
 * on the right (JUSTIFIED RIGHT: the mirror image) */
void cob_move_alnum(const void *src, int slen, void *dst, int dlen, int just)
{
    int n = slen < dlen ? slen : dlen;
    if (!just) {
        memmove(dst, src, n);
        if (n < dlen) memset((char *)dst + n, ' ', dlen - n);
    } else {
        memmove((char *)dst + dlen - n, (const char *)src + slen - n, n);
        if (n < dlen) memset(dst, ' ', dlen - n);
    }
}

/* The digits of a numeric item as unsigned DISPLAY characters (what a
 * numeric-to-alphanumeric MOVE sends: no sign, no point). */
static int num_to_digits(const void *p, const cob_desc *d, char *out)
{
    if (d->usage == COB_U_DISPLAY && !(d->flags & (COB_F_SEPLEAD | COB_F_SEPTRAIL))) {
        memcpy(out, p, d->size);
        unsigned char last = (unsigned char)out[d->size - 1];
        if (last >= 'p' && last <= 'y') out[d->size - 1] = (char)(last - 'p' + '0');
        return (int)d->size;
    }
    long long v = cob_get_num(p, d);
    unsigned long long mag = v < 0 ? (unsigned long long)(-v) : (unsigned long long)v;
    int digits = (d->flags & COB_F_NOTRUNC) ? capacity_digits(d->size) : d->digits;
    for (int i = digits - 1; i >= 0; i--) { out[i] = (char)('0' + mag % 10); mag /= 10; }
    return digits;
}

void cob_move(const void *src, const cob_desc *sd, void *dst, const cob_desc *dd)
{
    char tmp[40];
    int dnum = dd->cat == COB_NUM, snum = sd->cat == COB_NUM;

    if (dd->cat == COB_NUM_ED || dd->cat == COB_ALNUM_ED)
        cob_fatal("edited MOVE is not implemented yet (stage 3)");

    if (!dnum) {
        if (!snum || dd->cat == COB_GROUP) {
            cob_move_alnum(src, (int)sd->size, dst, (int)dd->size, dd->flags & COB_F_JUST);
        } else {
            int n = num_to_digits(src, sd, tmp);
            cob_move_alnum(tmp, n, dst, (int)dd->size, dd->flags & COB_F_JUST);
        }
        return;
    }

    if (snum) {
        cob_put_num(dst, dd, cob_get_num(src, sd), sd->scale);
        return;
    }

    /* alphanumeric (or group) to numeric: not a standard-conforming MOVE;
     * GnuCOBOL performs it as a numeric conversion of the text, treating
     * the sender as an unsigned DISPLAY integer of its own length. */
    cob_desc td = *sd;
    td.cat = COB_NUM; td.usage = COB_U_DISPLAY; td.scale = 0; td.flags = 0;
    const char *s = src;
    if (td.size > 18) { s += td.size - 18; td.size = 18; }
    td.digits = (unsigned char)td.size;
    cob_put_num(dst, dd, cob_get_num(s, &td), 0);
}

void cob_fill(void *dst, int n, int c) { memset(dst, c, n); }

void cob_fill_all(void *dst, int n, const char *lit, int len)
{
    char *d = dst;
    for (int i = 0; i < n; i++) d[i] = lit[i % len];
}

/* ---- comparison ------------------------------------------------------- */

static int cmp_bytes(const unsigned char *a, int na, const unsigned char *b, int nb)
{
    int n = na > nb ? na : nb;          /* the shorter is extended with spaces */
    for (int i = 0; i < n; i++) {
        int ca = i < na ? a[i] : ' ', cb = i < nb ? b[i] : ' ';
        if (ca != cb) return ca < cb ? -1 : 1;
    }
    return 0;
}

static int cmp_scaled(long long a, int sa, long long b, int sb)
{
    if (sa < sb) a *= pow10tab[sb - sa];
    else if (sb < sa) b *= pow10tab[sa - sb];
    return a < b ? -1 : a > b ? 1 : 0;
}

/* -1, 0, 1 */
int cob_cmp(const void *a, const cob_desc *ad, const void *b, const cob_desc *bd)
{
    int an = ad->cat == COB_NUM, bn = bd->cat == COB_NUM;
    if (an && bn) return cmp_scaled(cob_get_num(a, ad), ad->scale, cob_get_num(b, bd), bd->scale);
    char ta[40], tb[40];
    const unsigned char *pa = a, *pb = b;
    int na = (int)ad->size, nb = (int)bd->size;
    if (an && ad->usage != COB_U_DISPLAY) { na = num_to_digits(a, ad, ta); pa = (unsigned char *)ta; }
    if (bn && bd->usage != COB_U_DISPLAY) { nb = num_to_digits(b, bd, tb); pb = (unsigned char *)tb; }
    return cmp_bytes(pa, na, pb, nb);
}

/* class conditions: 0 NUMERIC, 1 ALPHABETIC, 2 ALPHABETIC-LOWER, 3 ALPHABETIC-UPPER */
int cob_class(const void *vp, const cob_desc *d, int kind)
{
    const unsigned char *p = vp;
    int n = (int)d->size;
    if (kind == 0) {
        if (d->cat == COB_NUM && d->usage != COB_U_DISPLAY) return 1;
        int start = (d->flags & COB_F_SEPLEAD) ? 1 : 0, end = (d->flags & COB_F_SEPTRAIL) ? n - 1 : n;
        for (int i = start; i < end; i++) {
            unsigned char c = p[i];
            if (c >= '0' && c <= '9') continue;
            if (i == n - 1 && (d->flags & COB_F_SIGNED) && c >= 'p' && c <= 'y') continue;
            return 0;
        }
        return 1;
    }
    for (int i = 0; i < n; i++) {
        unsigned char c = p[i];
        if (c == ' ') continue;
        int lo = (c >= 'a' && c <= 'z'), up = (c >= 'A' && c <= 'Z');
        if (kind == 1 && !(lo || up)) return 0;
        if (kind == 2 && !lo) return 0;
        if (kind == 3 && !up) return 0;
    }
    return 1;
}

/* ---- the numeric stack: ADD/SUBTRACT/MULTIPLY/DIVIDE, COMPUTE later --- */

typedef struct { long long v; int scale; } cob_num;
static cob_num nstk[32];
static int nsp;

void cob_push(const void *p, const cob_desc *d)
{
    if (nsp >= 32) cob_fatal("numeric stack overflow");
    nstk[nsp].v = cob_get_num(p, d); nstk[nsp].scale = d->scale; nsp++;
}

void cob_push_lit(long long v, int scale)
{
    if (nsp >= 32) cob_fatal("numeric stack overflow");
    nstk[nsp].v = v; nstk[nsp].scale = scale; nsp++;
}

static void align2(cob_num *a, cob_num *b)
{
    if (a->scale < b->scale) { a->v *= pow10tab[b->scale - a->scale]; a->scale = b->scale; }
    else if (b->scale < a->scale) { b->v *= pow10tab[a->scale - b->scale]; b->scale = a->scale; }
}

void cob_nadd(void) { cob_num *a = &nstk[nsp - 2], *b = &nstk[nsp - 1]; align2(a, b); a->v += b->v; nsp--; }
void cob_nsub(void) { cob_num *a = &nstk[nsp - 2], *b = &nstk[nsp - 1]; align2(a, b); a->v -= b->v; nsp--; }
void cob_nmul(void) { cob_num *a = &nstk[nsp - 2], *b = &nstk[nsp - 1]; a->v *= b->v; a->scale += b->scale; nsp--; }

/* Division carries the operands' larger scale plus guard digits, so a
 * receiver with a wider scale than either operand still gets its digits;
 * the store truncates.  (The 85 intermediate rules are implementor-defined;
 * this is the stage-2 rule and stage 3 may tighten it.) */
void cob_ndiv(void)
{
    cob_num *a = &nstk[nsp - 2], *b = &nstk[nsp - 1];
    if (b->v == 0) cob_fatal("division by zero (ON SIZE ERROR not implemented yet)");
    int guard = 6;
    int target = (a->scale > b->scale ? a->scale : b->scale) + guard;
    long long av = a->v, bv = b->v;
    int raise = target + b->scale - a->scale;
    while (raise > 0 && av < pow10tab[17] && av > -pow10tab[17]) { av *= 10; raise--; }
    while (raise > 0) { bv /= 10; raise--; }
    a->v = av / bv; a->scale = target;
    nsp--;
}

void cob_nneg(void) { nstk[nsp - 1].v = -nstk[nsp - 1].v; }

void cob_top_store(void *p, const cob_desc *d) { cob_put_num(p, d, nstk[nsp - 1].v, nstk[nsp - 1].scale); }

void cob_top_addto(void *p, const cob_desc *d)
{
    cob_num a = { cob_get_num(p, d), d->scale }, b = nstk[nsp - 1];
    align2(&a, &b);
    cob_put_num(p, d, a.v + b.v, a.scale);
}

void cob_top_subfrom(void *p, const cob_desc *d)
{
    cob_num a = { cob_get_num(p, d), d->scale }, b = nstk[nsp - 1];
    align2(&a, &b);
    cob_put_num(p, d, a.v - b.v, a.scale);
}

void cob_drop(void) { if (nsp) nsp--; }

/* subscripts: the integer value of an item */
int cob_load_int(const void *p, const cob_desc *d)
{
    long long v = cob_get_num(p, d);
    if (d->scale > 0) v /= pow10tab[d->scale];
    return (int)v;
}

/* ---- PERFORM ---------------------------------------------------------- */

/* PERFORM p THRU q pushes (q's id, return address); the code at the end
 * of every paragraph and section asks whether it is the top exit and, if
 * so, pops and returns there.  Nested and recursive PERFORMs behave like
 * GnuCOBOL's frame stack, not like a single exit cell. */
typedef struct { int exit_id; void *ret; } cob_frame;
static cob_frame pstk[256];
static int psp;

void cob_perform_push(int exit_id, void *ret)
{
    if (psp >= 256) cob_fatal("PERFORM nesting too deep");
    pstk[psp].exit_id = exit_id; pstk[psp].ret = ret; psp++;
}

void *cob_perform_exit(int id)
{
    if (psp > 0 && pstk[psp - 1].exit_id == id) return pstk[--psp].ret;
    return 0;
}
