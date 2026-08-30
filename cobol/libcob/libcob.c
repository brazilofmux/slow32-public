/* libcob -- the SLOW-32 COBOL runtime.
 *
 * Guest code, built by the SLOW-32 C toolchain (cobol/ is in the tree's
 * ordinary universe, so the host compiles it).  The compiler lowers each
 * verb either to a short inline sequence or to a call in here with a
 * descriptor it built (cobrt.h); the runtime works in bytes and pictures
 * and knows nothing about the statement that called it.
 *
 * Stage 2: DISPLAY; MOVE across the conversion matrix; comparison; class
 * tests; a scaled-i64 numeric stack for the arithmetic statements; the
 * PERFORM stack.  Stage 3: editing and de-editing (cobedit.h), ROUNDED,
 * SIZE ERROR, COMPUTE's operators.  Stage 4: line sequential and fixed
 * sequential files, STRING, the case intrinsics.  Stage 5: indexed files
 * on the default path (docs/indexed.md).  Stage 7: Report Writer, the
 * cheap half (docs/report-writer.md).  Stage 8: SCREEN SECTION on the
 * term service (docs/screen.md).  Stage 9: INSPECT, reference
 * modification, CURRENT-DATE -- what menu and taskdt drag in.  Stage 10:
 * sequential V with the IBM RDW (docs/framing.md).  Stage 13: the
 * command line -- ARGUMENT-NUMBER, ARGUMENT-VALUE, COMMAND-LINE.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cobrt.h"
#include "cobedit.h"
#include <term.h>
#include <time.h>

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

static int term_up;      /* the terminal service is initialised and raw */

static void term_down(void)
{
    if (!term_up) return;
    term_set_attr(0);
    term_set_raw(0);
    term_cleanup();
    term_up = 0;
}

void cob_stop_run(int code)
{
    out_flush();
    term_down();
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

    if (d->cat == COB_NUM_ED) {
        /* de-editing: a 1985 feature IBM ANS COBOL never had */
        char digs[40];
        int n = cob_deedit(d->pic, p, digs, &neg);
        for (int i = 0; i < n; i++) v = v * 10 + (digs[i] - '0');
        return neg ? -v : v;
    }

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
/* opts: 1 = ROUNDED (nearest, ties away from zero -- the 85 rule),
 * 2 = report a size error instead of truncating.  Returns 1 on a size
 * error (nothing stored), else 0. */
int cob_put_num_x(void *vp, const cob_desc *d, long long v, int vscale, int opts)
{
    unsigned char *p = vp;
    if (vscale > d->scale) {
        long long k = pow10tab[vscale - d->scale];
        long long q = v / k, r = v % k;
        if ((opts & 1) && (r < 0 ? -r : r) * 2 >= k) q += (v < 0) ? -1 : 1;
        v = q;
    } else if (vscale < d->scale) v *= pow10tab[d->scale - vscale];

    int neg = v < 0;
    unsigned long long mag = neg ? (unsigned long long)(-v) : (unsigned long long)v;
    if (d->flags & COB_F_NOTRUNC) {
        if ((opts & 2) && d->size < 8) {
            unsigned long long lim = 1ULL << (d->size * 8 - ((d->flags & COB_F_SIGNED) ? 1 : 0));
            if (mag >= lim) return 1;
        }
    } else if (d->digits <= 18) {
        if ((opts & 2) && mag >= (unsigned long long)pow10tab[d->digits]) return 1;
        mag %= (unsigned long long)pow10tab[d->digits];
    }
    if (!(d->flags & COB_F_SIGNED)) neg = 0;         /* unsigned takes the magnitude */

    if (d->cat == COB_NUM_ED) {
        char digs[40];
        for (int i = d->digits - 1; i >= 0; i--) { digs[i] = (char)('0' + mag % 10); mag /= 10; }
        cob_edit_apply(d->pic, digs, neg, d->flags & COB_F_BLANKZ, (char *)p);
        return 0;
    }

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
    return 0;
}

void cob_put_num(void *vp, const cob_desc *d, long long v, int vscale) { cob_put_num_x(vp, d, v, vscale, 0); }

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

/* alphanumeric-edited receiver: source characters into the A/X/9
 * positions, insertion characters where the picture puts them */
static void move_alnum_edited(const char *s, int n, char *dst, const cob_desc *dd)
{
    int si = 0, o = 0;
    for (const char *p = dd->pic; *p; p++) {
        switch (*p) {
        case 'B': dst[o++] = ' '; break;
        case '0': dst[o++] = '0'; break;
        case '/': dst[o++] = '/'; break;
        default:  dst[o++] = si < n ? s[si++] : ' '; break;
        }
    }
}

void cob_move(const void *src, const cob_desc *sd, void *dst, const cob_desc *dd)
{
    char tmp[40];
    int dnum = dd->cat == COB_NUM || dd->cat == COB_NUM_ED;
    int snum = sd->cat == COB_NUM || sd->cat == COB_NUM_ED;

    if (dd->cat == COB_ALNUM_ED) {
        if (!snum || sd->cat == COB_NUM_ED) move_alnum_edited(src, (int)sd->size, dst, dd);
        else { int n = num_to_digits(src, sd, tmp); move_alnum_edited(tmp, n, dst, dd); }
        return;
    }

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

    /* alphanumeric (or group) to numeric: not a standard-conforming MOVE.
     * GnuCOBOL (measured) reads the text as a decimal number -- blanks
     * skipped, an optional sign, digits, an optional point and fraction,
     * anything else ending it, no digits at all meaning zero -- and stores
     * it with the receiver's scale and digits.  usescreen's typed amount
     * relies on exactly this. */
    const char *s = src;
    unsigned n = sd->size, i = 0;
    while (i < n && s[i] == ' ') i++;
    int neg = 0;
    if (i < n && (s[i] == '+' || s[i] == '-')) { neg = (s[i] == '-'); i++; }
    long long v = 0; int scale = 0, seen_point = 0, digits = 0;
    for (; i < n; i++) {
        char c = s[i];
        if (c >= '0' && c <= '9') { if (digits < 18) { v = v * 10 + (c - '0'); digits++; if (seen_point) scale++; } }
        else if (c == '.' && !seen_point) seen_point = 1;
        else break;
    }
    cob_put_num(dst, dd, neg ? -v : v, scale);
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
static int div0;        /* a division by zero happened in this statement */

void cob_ndiv(void)
{
    cob_num *a = &nstk[nsp - 2], *b = &nstk[nsp - 1];
    if (b->v == 0) { div0 = 1; nsp--; return; }   /* size error; the left operand stands in */
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

/* a ** b for an integer b >= 0; anything else is beyond this stage */
void cob_npow(void)
{
    cob_num *a = &nstk[nsp - 2], *b = &nstk[nsp - 1];
    if (b->scale > 0) { long long k = pow10tab[b->scale]; if (b->v % k) cob_fatal("** with a non-integer exponent is not implemented"); b->v /= k; b->scale = 0; }
    if (b->v < 0) cob_fatal("** with a negative exponent is not implemented");
    long long base = a->v, r = 1; int scale = 0;
    for (long long i = 0; i < b->v; i++) {
        r *= base; scale += a->scale;
        while (scale > 12) { r /= 10; scale--; }
    }
    a->v = r; a->scale = scale;
    nsp--;
}

/* compare the two on top; pops both; -1 0 1 */
int cob_ncmp(void)
{
    cob_num *a = &nstk[nsp - 2], *b = &nstk[nsp - 1];
    align2(a, b);
    int r = a->v < b->v ? -1 : a->v > b->v ? 1 : 0;
    nsp -= 2;
    return r;
}

/* opts as cob_put_num_x; return 1 on a size error (receiver unchanged) */
int cob_top_store(void *p, const cob_desc *d, int opts)
{
    if (div0) return 1;
    return cob_put_num_x(p, d, nstk[nsp - 1].v, nstk[nsp - 1].scale, opts);
}

int cob_top_addto(void *p, const cob_desc *d, int opts)
{
    if (div0) return 1;
    cob_num a = { cob_get_num(p, d), d->scale }, b = nstk[nsp - 1];
    align2(&a, &b);
    return cob_put_num_x(p, d, a.v + b.v, a.scale, opts);
}

int cob_top_subfrom(void *p, const cob_desc *d, int opts)
{
    if (div0) return 1;
    cob_num a = { cob_get_num(p, d), d->scale }, b = nstk[nsp - 1];
    align2(&a, &b);
    return cob_put_num_x(p, d, a.v - b.v, a.scale, opts);
}

void cob_drop(void) { if (nsp) nsp--; div0 = 0; }

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

/* ====================================================================== */
/* Files                                                                   */
/* ====================================================================== */

/* Line sequential and fixed sequential.  The framing is the FD's fact
 * (docs/framing.md): line sequential is payload then '\n', trailing
 * spaces removed on WRITE (GnuCOBOL's convention, and csv2fw's), the
 * record area space-filled beyond a short line on READ, a '\r' before
 * the '\n' dropped.  A line longer than the record area is truncated with
 * status 04 -- not split into further records as GnuCOBOL 4 does. */

static void set_status(cob_file *f, const char *st)
{
    if (f->status) { f->status[0] = st[0]; f->status[1] = st[1]; }
}

/* 0 success, 1 at end / no record, 2 error.  A hard error with no FILE
 * STATUS to report it in stops the run, as GnuCOBOL does. */
static int file_result(cob_file *f, const char *st, const char *what)
{
    set_status(f, st);
    if (st[0] == '0') return 0;
    if (st[0] == '1' || st[0] == '2') return 1;      /* at end; the invalid key condition */
    if (!f->status) {
        char msg[96];
        int n = 0;
        const char *pre = "file error (status ";
        while (*pre) msg[n++] = *pre++;
        msg[n++] = st[0]; msg[n++] = st[1]; msg[n++] = ')'; msg[n++] = ' ';
        while (*what && n < 90) msg[n++] = *what++;
        msg[n] = 0;
        cob_fatal(msg);
    }
    return 2;
}

static const char *file_name(cob_file *f)
{
    static char name[256];
    if (f->assign) return f->assign;
    int n = (int)f->assign_len;
    if (n > 255) n = 255;
    while (n > 0 && f->assign_item[n - 1] == ' ') n--;
    memcpy(name, f->assign_item, n); name[n] = 0;
    return name;
}

static int idx_open(cob_file *f, int mode);
static int idx_close(cob_file *f);
static int idx_read_next(cob_file *f);
static int idx_write(cob_file *f);

int cob_open(cob_file *f, int mode)
{
    if (f->open_mode) return file_result(f, "41", "OPEN of a file already open");
    if (f->org == COB_ORG_INDEXED) return idx_open(f, mode);
    if (f->org == COB_ORG_RELATIVE) cob_fatal("RELATIVE files are not implemented yet (after v1)");
    const char *name = file_name(f);
    const char *fm = mode == COB_OPEN_INPUT ? "rb" : mode == COB_OPEN_OUTPUT ? "wb"
                   : mode == COB_OPEN_EXTEND ? "ab" : "r+b";
    FILE *fp = fopen(name, fm);
    f->at_eof = 0;
    if (!fp) {
        if (mode == COB_OPEN_INPUT && f->optional) {
            /* OPTIONAL and absent: open succeeds, the first READ is at end */
            f->open_mode = (unsigned char)mode; f->fp = 0; f->at_eof = 1;
            return file_result(f, "05", name);
        }
        if (mode == COB_OPEN_INPUT) return file_result(f, "35", name);
        return file_result(f, "30", name);
    }
    f->fp = fp; f->open_mode = (unsigned char)mode;
    return file_result(f, "00", name);
}

int cob_close(cob_file *f)
{
    if (!f->open_mode) return file_result(f, "42", "CLOSE of a file not open");
    if (f->org == COB_ORG_INDEXED) return idx_close(f);
    if (f->fp) fclose((FILE *)f->fp);
    f->fp = 0; f->open_mode = 0; f->at_eof = 0;
    return file_result(f, "00", "");
}

int cob_read(cob_file *f)
{
    if (!f->open_mode) return file_result(f, "47", "READ of a file not open");
    if (f->open_mode == COB_OPEN_OUTPUT || f->open_mode == COB_OPEN_EXTEND)
        return file_result(f, "47", "READ of a file open for output");
    if (f->org == COB_ORG_INDEXED) return idx_read_next(f);
    if (f->at_eof) return file_result(f, "10", "");
    FILE *fp = (FILE *)f->fp;
    char *rec = f->record;
    unsigned n = f->recsize;

    if (f->org == COB_ORG_SEQ && f->varying) {
        /* mode V: the four-byte RDW tapemgr writes -- 2 bytes big-endian
         * length including the RDW, 2 zero bytes -- then the payload.  The
         * record area beyond a short record is left as it was (cobc370's
         * rule: move, do not promise the tail). */
        unsigned char rdw[4];
        size_t got = fread(rdw, 1, 4, fp);
        if (got == 0) { f->at_eof = 1; return file_result(f, "10", ""); }
        if (got < 4) return file_result(f, "30", "truncated RDW");
        unsigned len = ((unsigned)rdw[0] << 8) | rdw[1];
        if (len < 4) return file_result(f, "30", "bad RDW");
        len -= 4;
        unsigned take = len < n ? len : n;
        if (fread(rec, 1, take, fp) != take) return file_result(f, "30", "truncated record");
        if (len > n) { fseek(fp, (long)(len - n), 1); }
        f->last_len = take;
        if (f->dep_item) cob_put_num(f->dep_item, (const cob_desc *)f->dep_desc, (long long)take, 0);
        return file_result(f, len > n ? "04" : "00", "");
    }
    if (f->org == COB_ORG_SEQ) {
        size_t got = fread(rec, 1, n, fp);
        if (got == 0) { f->at_eof = 1; return file_result(f, "10", ""); }
        if (got < n) { memset(rec + got, ' ', n - got); f->last_len = (unsigned)got; return file_result(f, "04", ""); }
        f->last_len = n;
        return file_result(f, "00", "");
    }

    unsigned i = 0; int c, truncated = 0, any = 0;
    while ((c = fgetc(fp)) != EOF) {
        any = 1;
        if (c == '\n') break;
        if (i < n) rec[i++] = (char)c; else truncated = 1;
    }
    if (!any) { f->at_eof = 1; return file_result(f, "10", ""); }
    if (i > 0 && rec[i - 1] == '\r') i--;
    f->last_len = i;
    if (i < n) memset(rec + i, ' ', n - i);
    return file_result(f, truncated ? "04" : "00", "");
}

/* before/after: extra newlines around the record (ADVANCING); reclen:
 * the size of the 01 the WRITE named, which is the length of a mode-V
 * record unless DEPENDING ON says otherwise */
int cob_write(cob_file *f, int before, int after, int reclen)
{
    if (!f->open_mode) return file_result(f, "48", "WRITE of a file not open");
    if (f->open_mode == COB_OPEN_INPUT) return file_result(f, "48", "WRITE of a file open for input");
    if (f->org == COB_ORG_INDEXED) return idx_write(f);
    FILE *fp = (FILE *)f->fp;
    const char *rec = f->record;
    unsigned n = f->recsize;
    if (f->org == COB_ORG_SEQ && f->varying) {
        unsigned len = reclen > 0 ? (unsigned)reclen : n;
        if (f->dep_item) {
            long long d = cob_get_num(f->dep_item, (const cob_desc *)f->dep_desc);
            if (d < (long long)f->minlen || d > (long long)n) return file_result(f, "44", "record length outside RECORD VARYING bounds");
            len = (unsigned)d;
        }
        unsigned char rdw[4] = { (unsigned char)((len + 4) >> 8), (unsigned char)((len + 4) & 255), 0, 0 };
        if (fwrite(rdw, 1, 4, fp) != 4 || fwrite(rec, 1, len, fp) != len) return file_result(f, "30", "write failed");
        return file_result(f, "00", "");
    }
    if (f->org == COB_ORG_SEQ) {
        if (fwrite(rec, 1, n, fp) != n) return file_result(f, "30", "write failed");
        return file_result(f, "00", "");
    }
    for (int i = 0; i < before; i++) fputc('\n', fp);
    while (n > 0 && rec[n - 1] == ' ') n--;
    if (n && fwrite(rec, 1, n, fp) != n) return file_result(f, "30", "write failed");
    fputc('\n', fp);
    for (int i = 0; i < after; i++) fputc('\n', fp);
    return file_result(f, "00", "");
}

/* ====================================================================== */
/* STRING                                                                  */
/* ====================================================================== */

static struct { char *dst; int dlen, pos, overflow; } cs;

/* pos is the 1-based POINTER value, or 0 when there is none */
void cob_str_begin(char *dst, int dlen, int pos)
{
    cs.dst = dst; cs.dlen = dlen; cs.overflow = 0;
    cs.pos = pos ? pos : 1;
    if (cs.pos < 1 || cs.pos > dlen) cs.overflow = 1;
}

/* delim of length dn; dn == 0 means DELIMITED BY SIZE */
void cob_str_src(const char *s, int n, const char *delim, int dn)
{
    if (cs.overflow) return;
    int take = n;
    if (dn) {
        for (int i = 0; i + dn <= n; i++)
            if (!memcmp(s + i, delim, dn)) { take = i; break; }
    }
    for (int i = 0; i < take; i++) {
        if (cs.pos > cs.dlen) { cs.overflow = 1; return; }
        cs.dst[cs.pos - 1] = s[i];
        cs.pos++;
    }
}

int cob_str_pointer(void) { return cs.pos; }
int cob_str_overflow(void) { return cs.overflow; }

/* an integer into any numeric item */
void cob_store_int(void *p, const cob_desc *d, int v) { cob_put_num(p, d, v, 0); }

/* ====================================================================== */
/* Intrinsic functions                                                     */
/* ====================================================================== */

static char fnbuf[4][1024];
static int fnrot;

static char *fn_buffer(int n)
{
    if (n > 1024) cob_fatal("intrinsic function argument longer than 1024");
    char *b = fnbuf[fnrot++ & 3];
    return b;
}

char *cob_fn_upper(const char *s, int n)
{
    char *b = fn_buffer(n);
    for (int i = 0; i < n; i++) b[i] = (s[i] >= 'a' && s[i] <= 'z') ? (char)(s[i] - 32) : s[i];
    return b;
}

char *cob_fn_lower(const char *s, int n)
{
    char *b = fn_buffer(n);
    for (int i = 0; i < n; i++) b[i] = (s[i] >= 'A' && s[i] <= 'Z') ? (char)(s[i] + 32) : s[i];
    return b;
}

/* ====================================================================== */
/* Indexed files: the default path                                          */
/* ====================================================================== */

/* On disk: the data file named by ASSIGN holds fixed slots of recsize
 * bytes (payload only -- no RDW, no delete byte), written in arrival
 * order; beside it, "<name>.key" holds the key table.  In memory the
 * table is an array of (key bytes, slot) kept sorted by key, so a random
 * READ is a binary search and READ NEXT walks the array.  Keys in
 * ascending arrival order (gl039's case) append without a sort; an
 * out-of-order WRITE inserts in place.  DELETE removes the entry and
 * leaves the slot unused.  The key file is rewritten on CLOSE.
 *
 *   key file: "S32KEY01" | u32 recsize | u32 keyoff | u32 keylen |
 *             u32 count | u32 nslots | u32 0 | u32 0 | count x (u32 slot, key)
 *
 * A btree can replace the array without changing the program-visible
 * behaviour; docs/indexed.md keeps the format's description. */

typedef struct {
    unsigned char *keys;    /* count entries of (keylen bytes + 4-byte slot), sorted */
    unsigned count, cap;
    unsigned nslots;        /* slots present in the data file */
    int pos;                /* current record pointer: next entry for READ NEXT; -1 none */
    int last;               /* entry the last READ delivered, for REWRITE/DELETE; -1 */
    int dirty;
} cob_idx;

#define KEYMAGIC "S32KEY01"

static unsigned entry_size(cob_file *f) { return f->keylen + 4; }
static unsigned char *entry(cob_file *f, cob_idx *x, unsigned i) { return x->keys + (size_t)i * entry_size(f); }
static unsigned entry_slot(cob_file *f, unsigned char *e) { unsigned char *p = e + f->keylen; return p[0] | (p[1] << 8) | (p[2] << 16) | ((unsigned)p[3] << 24); }
static void entry_set_slot(cob_file *f, unsigned char *e, unsigned s) { unsigned char *p = e + f->keylen; p[0] = (unsigned char)s; p[1] = (unsigned char)(s >> 8); p[2] = (unsigned char)(s >> 16); p[3] = (unsigned char)(s >> 24); }

static const char *key_file_name(cob_file *f)
{
    static char name[300];
    const char *d = file_name(f);
    size_t n = strlen(d);
    if (n > 290) n = 290;
    memcpy(name, d, n); memcpy(name + n, ".key", 5);
    return name;
}

/* binary search: index of the first entry with key >= k; *found says equal */
static unsigned idx_find(cob_file *f, cob_idx *x, const unsigned char *k, int *found)
{
    unsigned lo = 0, hi = x->count;
    while (lo < hi) {
        unsigned mid = (lo + hi) / 2;
        int c = memcmp(entry(f, x, mid), k, f->keylen);
        if (c < 0) lo = mid + 1; else hi = mid;
    }
    *found = (lo < x->count && !memcmp(entry(f, x, lo), k, f->keylen));
    return lo;
}

static void idx_grow(cob_file *f, cob_idx *x)
{
    if (x->count < x->cap) return;
    unsigned ncap = x->cap ? x->cap * 2 : 256;
    unsigned char *nk = realloc(x->keys, (size_t)ncap * entry_size(f));
    if (!nk) cob_fatal("out of memory for the key table");
    x->keys = nk; x->cap = ncap;
}

static void put_u32(unsigned char *p, unsigned v) { p[0] = (unsigned char)v; p[1] = (unsigned char)(v >> 8); p[2] = (unsigned char)(v >> 16); p[3] = (unsigned char)(v >> 24); }
static unsigned get_u32(const unsigned char *p) { return p[0] | (p[1] << 8) | (p[2] << 16) | ((unsigned)p[3] << 24); }

static int idx_load(cob_file *f, cob_idx *x)
{
    FILE *kf = fopen(key_file_name(f), "rb");
    if (!kf) return 0;
    unsigned char h[32];
    if (fread(h, 1, 32, kf) != 32 || memcmp(h, KEYMAGIC, 8) || get_u32(h + 8) != f->recsize ||
        get_u32(h + 12) != f->keyoff || get_u32(h + 16) != f->keylen) { fclose(kf); return 0; }
    x->count = get_u32(h + 20); x->nslots = get_u32(h + 24);
    x->cap = x->count ? x->count : 1;
    x->keys = malloc((size_t)x->cap * entry_size(f));
    if (!x->keys) cob_fatal("out of memory for the key table");
    size_t want = (size_t)x->count * entry_size(f);
    if (fread(x->keys, 1, want, kf) != want) { fclose(kf); return 0; }
    fclose(kf);
    return 1;
}

static int idx_save(cob_file *f, cob_idx *x)
{
    FILE *kf = fopen(key_file_name(f), "wb");
    if (!kf) return 0;
    unsigned char h[32];
    memset(h, 0, 32); memcpy(h, KEYMAGIC, 8);
    put_u32(h + 8, f->recsize); put_u32(h + 12, f->keyoff); put_u32(h + 16, f->keylen);
    put_u32(h + 20, x->count); put_u32(h + 24, x->nslots);
    fwrite(h, 1, 32, kf);
    size_t n = (size_t)x->count * entry_size(f);
    if (n && fwrite(x->keys, 1, n, kf) != n) { fclose(kf); return 0; }
    fclose(kf);
    return 1;
}

static int idx_open(cob_file *f, int mode)
{
    const char *name = file_name(f);
    if (f->keylen == 0 || f->keylen > 255) cob_fatal("RECORD KEY must be 1 to 255 bytes");
    cob_idx *x = calloc(1, sizeof *x);
    if (!x) cob_fatal("out of memory");
    x->pos = -1; x->last = -1;
    FILE *fp;
    if (mode == COB_OPEN_OUTPUT) {
        fp = fopen(name, "w+b");
        if (!fp) { free(x); return file_result(f, "30", name); }
    } else {
        fp = fopen(name, mode == COB_OPEN_INPUT ? "rb" : "r+b");
        if (!fp) {
            free(x);
            if (f->optional && mode != COB_OPEN_EXTEND) { f->open_mode = (unsigned char)mode; f->fp = 0; f->at_eof = 1; return file_result(f, "05", name); }
            return file_result(f, "35", name);
        }
        if (!idx_load(f, x)) { fclose(fp); free(x); return file_result(f, "39", "key file missing or does not match the FD"); }
    }
    f->fp = fp; f->idx = x; f->open_mode = (unsigned char)mode; f->at_eof = 0;
    return file_result(f, "00", name);
}

static int idx_close(cob_file *f)
{
    cob_idx *x = f->idx;
    int ok = 1;
    if (x) {
        if (f->open_mode != COB_OPEN_INPUT) ok = idx_save(f, x);
        free(x->keys); free(x);
    }
    if (f->fp) fclose((FILE *)f->fp);
    f->fp = 0; f->idx = 0; f->open_mode = 0; f->at_eof = 0;
    return file_result(f, ok ? "00" : "30", "could not write the key file");
}

static int slot_read(cob_file *f, unsigned slot)
{
    FILE *fp = (FILE *)f->fp;
    if (fseek(fp, (long)slot * (long)f->recsize, 0) != 0) return 0;
    return fread(f->record, 1, f->recsize, fp) == f->recsize;
}

static int slot_write(cob_file *f, unsigned slot)
{
    FILE *fp = (FILE *)f->fp;
    if (fseek(fp, (long)slot * (long)f->recsize, 0) != 0) return 0;
    return fwrite(f->record, 1, f->recsize, fp) == f->recsize;
}

/* WRITE: insert by the key in the record area */
static int idx_write(cob_file *f)
{
    cob_idx *x = f->idx;
    if (!x) return file_result(f, "48", "WRITE to an OPTIONAL file that is absent");
    const unsigned char *k = (const unsigned char *)f->record + f->keyoff;
    int found;
    unsigned at = idx_find(f, x, k, &found);
    if (found) return file_result(f, "22", "");                 /* duplicate key */
    if (f->access == 0 && x->count && at != x->count)
        return file_result(f, "21", "");                        /* sequential access: keys must ascend */
    unsigned slot = x->nslots;
    if (!slot_write(f, slot)) return file_result(f, "30", "write failed");
    x->nslots++;
    idx_grow(f, x);
    unsigned es = entry_size(f);
    memmove(entry(f, x, at + 1), entry(f, x, at), (size_t)(x->count - at) * es);
    memcpy(entry(f, x, at), k, f->keylen);
    entry_set_slot(f, entry(f, x, at), slot);
    x->count++;
    x->pos = (int)at + 1; x->last = -1;
    return file_result(f, "00", "");
}

/* READ with KEY (random): the key is what the record's key field holds */
int cob_read_key(cob_file *f)
{
    if (!f->open_mode) return file_result(f, "47", "READ of a file not open");
    if (f->org != COB_ORG_INDEXED) cob_fatal("READ ... KEY on a file that is not INDEXED");
    cob_idx *x = f->idx;
    if (!x) return file_result(f, "23", "");
    unsigned char key[256];
    memcpy(key, f->record + f->keyoff, f->keylen);
    int found;
    unsigned at = idx_find(f, x, key, &found);
    if (!found) { x->last = -1; return file_result(f, "23", ""); }
    if (!slot_read(f, entry_slot(f, entry(f, x, at)))) return file_result(f, "30", "read failed");
    x->pos = (int)at + 1; x->last = (int)at;
    return file_result(f, "00", "");
}

static int idx_read_next(cob_file *f)
{
    cob_idx *x = f->idx;
    if (!x || f->at_eof) return file_result(f, "10", "");
    if (x->pos < 0) x->pos = 0;
    if ((unsigned)x->pos >= x->count) { f->at_eof = 1; x->last = -1; return file_result(f, "10", ""); }
    if (!slot_read(f, entry_slot(f, entry(f, x, (unsigned)x->pos)))) return file_result(f, "30", "read failed");
    x->last = x->pos; x->pos++;
    return file_result(f, "00", "");
}

/* START: position by the key in the record area.  op: 0 =, 1 >, 2 >=, 3 <, 4 <= */
int cob_start(cob_file *f, int op)
{
    if (!f->open_mode) return file_result(f, "47", "START of a file not open");
    if (f->org != COB_ORG_INDEXED) cob_fatal("START on a file that is not INDEXED");
    cob_idx *x = f->idx;
    if (!x) return file_result(f, "23", "");
    const unsigned char *k = (const unsigned char *)f->record + f->keyoff;
    int found;
    unsigned at = idx_find(f, x, k, &found);
    int pos = -1;
    switch (op) {
    case 0: if (found) pos = (int)at; break;
    case 1: pos = (int)(found ? at + 1 : at); if ((unsigned)pos >= x->count) pos = -1; break;
    case 2: if (at < x->count) pos = (int)at; break;
    case 3: if (at > 0) pos = (int)at - 1; break;
    case 4: if (found) pos = (int)at; else if (at > 0) pos = (int)at - 1; break;
    }
    if (pos < 0) return file_result(f, "23", "");
    x->pos = pos; x->last = -1; f->at_eof = 0;
    return file_result(f, "00", "");
}

/* REWRITE: indexed by key; fixed sequential in place after a READ */
int cob_rewrite(cob_file *f)
{
    if (!f->open_mode) return file_result(f, "49", "REWRITE of a file not open");
    if (f->open_mode != COB_OPEN_IO) return file_result(f, "49", "REWRITE needs OPEN I-O");
    if (f->org == COB_ORG_INDEXED) {
        cob_idx *x = f->idx;
        const unsigned char *k = (const unsigned char *)f->record + f->keyoff;
        int found;
        unsigned at = idx_find(f, x, k, &found);
        if (!found) return file_result(f, "23", "");
        if (!slot_write(f, entry_slot(f, entry(f, x, at)))) return file_result(f, "30", "write failed");
        return file_result(f, "00", "");
    }
    if (f->org == COB_ORG_SEQ) {
        FILE *fp = (FILE *)f->fp;
        if (fseek(fp, -(long)f->recsize, 1) != 0) return file_result(f, "43", "");
        if (fwrite(f->record, 1, f->recsize, fp) != f->recsize) return file_result(f, "30", "write failed");
        return file_result(f, "00", "");
    }
    return file_result(f, "49", "REWRITE on a LINE SEQUENTIAL file");
}

/* DELETE: the record whose key is in the record area (random) or the
 * one last read (sequential access) */
int cob_delete(cob_file *f)
{
    if (!f->open_mode) return file_result(f, "49", "DELETE of a file not open");
    if (f->open_mode != COB_OPEN_IO) return file_result(f, "49", "DELETE needs OPEN I-O");
    if (f->org != COB_ORG_INDEXED) cob_fatal("DELETE on a file that is not INDEXED");
    cob_idx *x = f->idx;
    unsigned at; int found;
    if (f->access == 0) {
        if (x->last < 0) return file_result(f, "43", "");
        at = (unsigned)x->last; found = 1;
    } else at = idx_find(f, x, (const unsigned char *)f->record + f->keyoff, &found);
    if (!found) return file_result(f, "23", "");
    unsigned es = entry_size(f);
    memmove(entry(f, x, at), entry(f, x, at + 1), (size_t)(x->count - at - 1) * es);
    x->count--;
    if (x->pos > (int)at) x->pos--;
    x->last = -1;
    return file_result(f, "00", "");
}

/* ====================================================================== */
/* Report Writer                                                           */
/* ====================================================================== */

/* The page model that reproduces GnuCOBOL's line-sequential print files
 * (measured on majesty's .prn: every page is exactly PAGE LIMIT physical
 * lines, blank lines fill the gaps and the tail, no form feed):
 *   - a page heading is presented when the first body group of a page
 *     is generated; an absolute LINE n lands on line n, a relative one on
 *     LINE-COUNTER + n;
 *   - the first body group on a page with a relative first line lands on
 *     FIRST DETAIL (the 85 rule), later ones on LINE-COUNTER + n;
 *   - a body group whose last line would pass LAST DETAIL advances the
 *     page first: blank lines to PAGE LIMIT, then the heading again;
 *   - TERMINATE pads the current page to PAGE LIMIT. */

#define RW_WIDTH 512
static char rw_line[RW_WIDTH];

static void rw_put_line(cob_report *r, const char *p, int n)
{
    cob_file *f = r->file;
    if (!f->open_mode || !f->fp) cob_fatal("GENERATE: the report's print file is not open");
    while (n > 0 && p[n - 1] == ' ') n--;
    if (n) fwrite(p, 1, n, (FILE *)f->fp);
    fputc('\n', (FILE *)f->fp);
    r->line_counter++;
}

static void rw_blank_to(cob_report *r, int line)   /* blank lines up to, not including, line */
{
    while (r->line_counter < line - 1) rw_put_line(r, "", 0);
}

void cob_rw_initiate(cob_report *r)
{
    r->line_counter = 0; r->page_counter = 0; r->body_seen = 0;
}

/* where the next line would land: a body line while no body group has
 * been presented on the page goes to FIRST DETAIL -- the 85 rule for
 * the first body group, and (measured on the activity report) where
 * GnuCOBOL puts a group's remaining lines when they spill onto a new
 * page */
static int rw_target(cob_report *r, int abs, int plus, int is_body)
{
    if (abs) return abs;
    if (is_body && !r->body_seen) return r->first_detail;
    return r->line_counter + plus;
}

/* 1 if the body group (first line abs/plus, `height` further lines of
 * relative extent) needs a new page before it is presented */
int cob_rw_fit(cob_report *r, int abs, int plus, int height)
{
    if (r->page_counter == 0) return 1;
    int first = rw_target(r, abs, plus, 1);
    return first + height > r->last_detail;
}

/* end the page: pad to PAGE LIMIT (when anything was printed), count it */
void cob_rw_page_end(cob_report *r)
{
    if (r->page_counter > 0) while (r->line_counter < r->page_limit) rw_put_line(r, "", 0);
    r->page_counter++;
    r->line_counter = 0; r->body_seen = 0;
}

void cob_rw_line_begin(void) { memset(rw_line, ' ', RW_WIDTH); }

void cob_rw_field(int col, const cob_desc *dd, const void *src, const cob_desc *sd)
{
    if (col < 1 || col - 1 + (int)dd->size > RW_WIDTH) cob_fatal("report line wider than 512 columns");
    cob_move(src, sd, rw_line + col - 1, dd);
}

void cob_rw_line_write(cob_report *r, int abs, int plus, int is_body)
{
    int target = rw_target(r, abs, plus, is_body);
    if (target < r->line_counter + 1) target = r->line_counter + 1;
    rw_blank_to(r, target);
    rw_put_line(r, rw_line, RW_WIDTH);
    if (is_body) r->body_seen = 1;
}

/* a body line that would land past LAST DETAIL spills onto a new page:
 * the compiler renders the heading, and the line then lands on FIRST
 * DETAIL.  Measured on the activity report (a group's trailing blank line
 * starting the next page) and the profit-and-loss report (the same, with
 * TERMINATE padding that page). */
int cob_rw_line_overflows(cob_report *r, int abs, int plus, int is_body)
{
    if (!is_body || r->page_counter == 0) return 0;
    return rw_target(r, abs, plus, is_body) > r->last_detail;
}

void cob_rw_terminate(cob_report *r)
{
    if (r->page_counter > 0) while (r->line_counter < r->page_limit) rw_put_line(r, "", 0);
}

/* ====================================================================== */
/* SCREEN SECTION                                                          */
/* ====================================================================== */

/* DISPLAY paints every slot; ACCEPT paints, then runs the focus loop
 * over the TO and USING slots in order (dBase Stage 4's READ, on the
 * same term service): printable keys overwrite and advance, Backspace
 * erases, Enter and Tab move to the next field, Escape ends the ACCEPT,
 * AUTO advances when the field fills.  Each input field's text is then
 * MOVEd into its item through the ordinary conversion matrix.
 * UNDERLINE has no term.h attribute yet: painted plain (screen.md). */

static void term_need(void)
{
    if (term_up) return;
    out_flush();
    if (term_init() != 0) cob_fatal("the terminal service is not available (run under an emulator with the term service)");
    term_set_raw(1);
    term_up = 1;
}

void cbl_get_scr_size(unsigned char *lines, unsigned char *cols)
{
    int r = 24, c = 80;
    term_need();
    term_get_size(&r, &c);
    *lines = (unsigned char)(r > 255 ? 255 : r);
    *cols = (unsigned char)(c > 255 ? 255 : c);
}

static void scr_attr(const cob_scr_field *f)
{
    if (f->flags & COB_SF_REVERSE) term_set_attr(7);
    else if (f->flags & COB_SF_HIGHLIGHT) term_set_attr(1);
    else term_set_attr(0);
}

static void scr_puts_n(const char *p, unsigned n)
{
    for (unsigned i = 0; i < n; i++) term_putc(p[i]);
}

/* render a FROM/USING item through its picture into buf (width bytes) */
static void scr_render(const cob_scr_field *f, char *buf)
{
    if (f->kind == COB_SCR_VALUE) { memcpy(buf, f->value, f->width); return; }
    if (f->kind == COB_SCR_TO) { memset(buf, ' ', f->width); return; }
    cob_move(f->item, (const cob_desc *)f->item_desc, buf, (const cob_desc *)f->pic);
}

static void scr_paint_field(const cob_scr_field *f)
{
    char buf[512];
    if (f->width > sizeof buf) cob_fatal("screen field wider than 512");
    scr_render(f, buf);
    term_gotoxy(f->line, f->col);
    scr_attr(f);
    scr_puts_n(buf, f->width);
    term_set_attr(0);
}

void cob_screen_display(const cob_screen *s)
{
    term_need();
    term_begin_update();
    if (s->blank_screen) term_clear(0);
    for (unsigned i = 0; i < s->nfields; i++) scr_paint_field(&s->fields[i]);
    term_end_update();
}

void cob_screen_accept(const cob_screen *s)
{
    cob_screen_display(s);
    /* edit buffers for the input fields */
    unsigned nin = 0;
    for (unsigned i = 0; i < s->nfields; i++)
        if (s->fields[i].kind == COB_SCR_TO || s->fields[i].kind == COB_SCR_USING) nin++;
    if (!nin) { term_getkey(); return; }        /* nothing to type into: wait for a key */
    char **ed = malloc(nin * sizeof *ed);
    unsigned *idx = malloc(nin * sizeof *idx);
    if (!ed || !idx) cob_fatal("out of memory");
    unsigned k = 0;
    for (unsigned i = 0; i < s->nfields; i++) {
        const cob_scr_field *f = &s->fields[i];
        if (f->kind != COB_SCR_TO && f->kind != COB_SCR_USING) continue;
        ed[k] = malloc(f->width + 1);
        if (!ed[k]) cob_fatal("out of memory");
        scr_render(f, ed[k]);
        idx[k++] = i;
    }
    unsigned cur = 0, pos = 0;
    int done = 0;
    while (!done) {
        const cob_scr_field *f = &s->fields[idx[cur]];
        term_gotoxy(f->line, f->col + (int)pos);
        int key = term_getkey();
        if (key == -1 || key == 27) { done = 1; break; }             /* EOF / Escape ends the ACCEPT */
        if (key == '\r' || key == '\n' || key == '\t') {
            if (cur + 1 < nin) { cur++; pos = 0; } else done = 1;
            continue;
        }
        if (key == 8 || key == 127) {
            if (pos > 0) { pos--; ed[cur][pos] = ' '; term_gotoxy(f->line, f->col + (int)pos); term_putc(' '); }
            continue;
        }
        if (key >= 32 && key < 127) {
            if (pos < f->width) {
                ed[cur][pos] = (char)key;
                term_putc((char)key);
                pos++;
                if (pos >= f->width && (f->flags & COB_SF_AUTO)) {
                    if (cur + 1 < nin) { cur++; pos = 0; } else done = 1;
                }
            }
            continue;
        }
        /* other control keys are ignored */
    }
    /* commit every input field into its item as alphanumeric text */
    for (unsigned i = 0; i < nin; i++) {
        const cob_scr_field *f = &s->fields[idx[i]];
        cob_desc td; memset(&td, 0, sizeof td);
        td.cat = COB_ALNUM; td.usage = COB_U_DISPLAY; td.size = f->width;
        cob_move(ed[i], &td, f->item, (const cob_desc *)f->item_desc);
        free(ed[i]);
    }
    free(ed); free(idx);
}

/* ====================================================================== */
/* Stage 9: INSPECT, reference modification, the clock                     */
/* ====================================================================== */

/* the integer value of the numeric stack's top; pops it */
int cob_pop_int(void)
{
    if (nsp <= 0) cob_fatal("numeric stack underflow");
    cob_num *a = &nstk[--nsp];
    long long v = a->v;
    if (a->scale > 0) v /= pow10tab[a->scale];
    return (int)v;
}

/* a descriptor for item(start:len): the base's category, the given
 * length (0: to the end of the item).  Rotating buffers, like the
 * intrinsic functions'. */
static cob_desc rmdesc[8];
static int rmrot;

const cob_desc *cob_refmod_desc(const cob_desc *base, int start, int len)
{
    if (start < 1 || (unsigned)start > base->size) cob_fatal("reference modification: start is outside the item");
    if (len == 0) len = (int)base->size - start + 1;
    if (len < 1 || (unsigned)(start - 1 + len) > base->size) cob_fatal("reference modification: length is outside the item");
    cob_desc *d = &rmdesc[rmrot++ & 7];
    memset(d, 0, sizeof *d);
    d->cat = (base->cat == COB_NUM && base->usage == COB_U_DISPLAY) || base->cat == COB_ALNUM || base->cat == COB_ALPHA ? COB_ALNUM : COB_ALNUM;
    d->usage = COB_U_DISPLAY;
    d->size = (unsigned)len;
    return d;
}

int cob_refmod_len(const cob_desc *base, int start, int len)
{
    if (len == 0) len = (int)base->size - start + 1;
    return len;
}

/* INSPECT ... TALLYING: kind 0 CHARACTERS, 1 ALL, 2 LEADING; the count
 * of occurrences in item[0..n) (BEFORE/AFTER INITIAL narrow n and the
 * start on the compiler side) */
int cob_inspect_tally(const char *p, int n, int kind, const char *pat, int plen)
{
    int count = 0;
    if (kind == 0) return n;
    if (plen < 1 || plen > n) return 0;
    for (int i = 0; i + plen <= n; ) {
        if (!memcmp(p + i, pat, plen)) { count++; i += plen; }
        else { if (kind == 2) break; i++; }
    }
    return count;
}

/* INSPECT ... REPLACING: kind 0 CHARACTERS, 1 ALL, 2 LEADING, 3 FIRST;
 * pattern and replacement are the same length (the standard's rule) */
void cob_inspect_replace(char *p, int n, int kind, const char *pat, int plen, const char *rep)
{
    if (kind == 0) { for (int i = 0; i < n; i++) p[i] = rep[0]; return; }
    if (plen < 1 || plen > n) return;
    for (int i = 0; i + plen <= n; ) {
        if (!memcmp(p + i, pat, plen)) { memcpy(p + i, rep, plen); i += plen; if (kind == 3) return; }
        else { if (kind == 2) break; i++; }
    }
}

/* FUNCTION CURRENT-DATE: YYYYMMDDhhmmsshh followed by the offset from
 * UTC as +hhmm / -hhmm (21 characters); the guest clock through the
 * emulator, local time as the guest libc gives it */
char *cob_fn_current_date(void)
{
    char *b = fn_buffer(21);
    struct timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
    int hund = 0;
    if (clock_gettime(0, &ts) == 0) hund = (int)(ts.tv_nsec / 10000000);   /* CLOCK_REALTIME: the hundredths */
    else ts.tv_sec = time(0);
    time_t now = (time_t)ts.tv_sec;
    struct tm *t = localtime(&now);
    int y = t->tm_year + 1900, mo = t->tm_mon + 1, d = t->tm_mday;
    long off = t->tm_gmtoff;
    int neg = off < 0; if (neg) off = -off;
    int oh = (int)(off / 3600), om = (int)((off % 3600) / 60);
    char tmp[32];
    int n = 0;
    #define PUT2(v) do { tmp[n++] = (char)('0' + (v) / 10 % 10); tmp[n++] = (char)('0' + (v) % 10); } while (0)
    tmp[n++] = (char)('0' + y / 1000 % 10); tmp[n++] = (char)('0' + y / 100 % 10); PUT2(y % 100);
    PUT2(mo); PUT2(d); PUT2(t->tm_hour); PUT2(t->tm_min); PUT2(t->tm_sec); PUT2(hund);
    tmp[n++] = neg ? '-' : '+'; PUT2(oh); PUT2(om);
    #undef PUT2
    memcpy(b, tmp, 21);
    return b;
}

/* ====================================================================== */
/* The command line: ACCEPT FROM ARGUMENT-NUMBER / ARGUMENT-VALUE /       */
/* COMMAND-LINE, DISPLAY UPON ARGUMENT-NUMBER (GnuCOBOL's implementor      */
/* module, measured: the count excludes the program name; ARGUMENT-VALUE   */
/* yields the arguments in turn from 1, DISPLAY n UPON ARGUMENT-NUMBER     */
/* makes the next one n, and past the end the item is left unchanged).     */
/* ====================================================================== */

static int cl_argc;
static char **cl_argv;
static int cl_next = 1;

void cob_set_args(int argc, char **argv) { cl_argc = argc; cl_argv = argv; cl_next = 1; }

static void put_text(void *p, const cob_desc *d, const char *s, int n)
{
    cob_desc td; memset(&td, 0, sizeof td);
    td.cat = COB_ALNUM; td.usage = COB_U_DISPLAY; td.size = (unsigned)n;
    cob_move(s, &td, p, d);
}

void cob_accept_argnum(void *p, const cob_desc *d)
{
    cob_put_num(p, d, cl_argc > 0 ? cl_argc - 1 : 0, 0);
}

void cob_display_upon_argnum(int n) { cl_next = n; }

void cob_accept_argval(void *p, const cob_desc *d)
{
    if (cl_next < 1 || cl_next >= cl_argc) return;            /* past the end: unchanged */
    const char *a = cl_argv[cl_next++];
    put_text(p, d, a, (int)strlen(a));
}

void cob_accept_cmdline(void *p, const cob_desc *d)
{
    char line[4096]; int n = 0;
    for (int i = 1; i < cl_argc; i++) {
        const char *a = cl_argv[i];
        if (i > 1 && n < (int)sizeof line) line[n++] = ' ';
        for (const char *q = a; *q && n < (int)sizeof line; q++) line[n++] = *q;
    }
    put_text(p, d, line, n);
}
