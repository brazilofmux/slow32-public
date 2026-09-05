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
#include <math.h>
#include <ctype.h>
#include "cobrt.h"
#include "cobedit.h"
#include <term.h>
#include <time.h>
#include "xsort.h"
#include "btree.h"

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

extern int cob_switches[8];
void cob_init(void)
{
    out_n = 0;
    /* SPECIAL-NAMES SWITCH-1..8 as the environment sets them: COB_SWITCH_n
     * = ON (GnuCOBOL's convention, which NIST's report.pl relies on) */
    for (int i = 0; i < 8; i++) {
        char name[16]; snprintf(name, sizeof name, "COB_SWITCH_%d", i + 1);
        const char *v = getenv(name);
        cob_switches[i] = v && (v[0] == 'O' || v[0] == 'o') && (v[1] == 'N' || v[1] == 'n');
    }
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

#define COB_RBUF 8192   /* line-sequential read buffer */

/* ---- digits without 64-bit division ---------------------------------- */
/* SLOW-32 divides 32 bits in hardware and 64 bits a bit at a time (some
 * 600 instructions), and a numeric item is at most 18 digits: so the
 * digit loops work nine digits at a time in a 32-bit word, two digits a
 * step through a pairs table, and a division by a power of ten is by a
 * constant in every case, which the compiler turns into a multiply. */
static const char digit_pairs[201] =
    "00010203040506070809" "10111213141516171819" "20212223242526272829"
    "30313233343536373839" "40414243444546474849" "50515253545556575859"
    "60616263646566676869" "70717273747576777879" "80818283848586878889"
    "90919293949596979899";

/* the low n digits of mag as characters, the least significant at out[n-1] */
static void mag_to_digits(unsigned long long mag, char *out, int n)
{
    while (n > 0) {
        unsigned lo; int take;
        if (mag >> 32) { unsigned long long q = mag / 1000000000ULL; lo = (unsigned)(mag - q * 1000000000ULL); mag = q; take = 9; }
        else { lo = (unsigned)mag; mag = 0; take = n; }
        if (take > n) take = n;
        n -= take;
        char *o = out + n + take;
        while (take >= 2) { unsigned q = lo / 100u, r = lo - q * 100u; *--o = digit_pairs[r * 2 + 1]; *--o = digit_pairs[r * 2]; lo = q; take -= 2; }
        if (take) *--o = (char)('0' + lo % 10u);
    }
}

/* a / 10^m, the remainder through *rem */
static unsigned long long udiv_pow10(unsigned long long a, int m, unsigned long long *rem)
{
    unsigned long long q;
    if (m <= 0) { *rem = 0; return a; }
    if (!(a >> 32) && m <= 9) { unsigned x = (unsigned)a, d = (unsigned)pow10tab[m], qq = x / d; *rem = x - qq * d; return qq; }
    switch (m) {
    case 1: q = a / 10ULL; break;
    case 2: q = a / 100ULL; break;
    case 3: q = a / 1000ULL; break;
    case 4: q = a / 10000ULL; break;
    case 5: q = a / 100000ULL; break;
    case 6: q = a / 1000000ULL; break;
    case 7: q = a / 10000000ULL; break;
    case 8: q = a / 100000000ULL; break;
    case 9: q = a / 1000000000ULL; break;
    case 10: q = a / 10000000000ULL; break;
    case 11: q = a / 100000000000ULL; break;
    case 12: q = a / 1000000000000ULL; break;
    case 13: q = a / 10000000000000ULL; break;
    case 14: q = a / 100000000000000ULL; break;
    case 15: q = a / 1000000000000000ULL; break;
    case 16: q = a / 10000000000000000ULL; break;
    case 17: q = a / 100000000000000000ULL; break;
    case 18: q = a / 1000000000000000000ULL; break;
    default: *rem = a; return 0;
    }
    *rem = a - q * (unsigned long long)pow10tab[m];
    return q;
}

/* v / 10^m with C's truncation toward zero, the remainder v's sign */
static long long div_pow10(long long v, int m, long long *rem)
{
    unsigned long long r, q = udiv_pow10(v < 0 ? 0 - (unsigned long long)v : (unsigned long long)v, m, &r);
    if (rem) *rem = v < 0 ? -(long long)r : (long long)r;
    return v < 0 ? -(long long)q : (long long)q;
}

/* Value of the item scaled by 10^scale (i.e. the integer the digits spell). */
/* de-editing: a 1985 feature IBM ANS COBOL never had.  Out of line so
 * its arrays do not put a 350-byte frame under every numeric fetch. */
static __attribute__((noinline)) long long get_num_edited(const unsigned char *p, const cob_desc *d)
{
    long long v = 0;
    int neg = 0;
    {
        char digs[40];
        unsigned char sw[256];
        if ((cob_dp_comma || cob_currency != '$') && d->size <= sizeof sw) {     /* the bytes carry ',' for the point, c for '$': read them the other way round */
            for (size_t i = 0; i < d->size; i++) {
                unsigned char c = p[i];
                if (cob_dp_comma) c = c == '.' ? ',' : c == ',' ? '.' : c;
                if (cob_currency != '$' && c == (unsigned char)cob_currency) c = '$';
                sw[i] = c;
            }
            p = sw;
        }
        int n = cob_deedit(d->pic, p, digs, &neg);
        for (int i = 0; i < n; i++) v = v * 10 + (digs[i] - '0');
        return neg ? -v : v;
    }
}

long long cob_get_num(const void *vp, const cob_desc *d)
{
    const unsigned char *p = vp;
    long long v = 0;
    int neg = 0;

    if (d->cat == COB_NUM_ED) return get_num_edited(p, d);

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
        /* eight digits at a time in a 32-bit word (the flush into v kept
         * out of the byte loop: inside it the compiler if-converts it into
         * a 64-bit multiply on every byte) */
        int i = 0, first = 1;
        while (i < bytes - 1) {
            int len = bytes - 1 - i < 4 ? bytes - 1 - i : 4, stop = i + len;
            unsigned w = 0;
            for (; i < stop; i++) w = w * 100 + (p[i] >> 4) * 10 + (p[i] & 15);
            v = first ? (long long)w : v * pow10tab[2 * len] + w; first = 0;
        }
        v = v * 10 + (p[bytes - 1] >> 4);
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
        /* nine digits at a time in a 32-bit word; the flush into v stays
         * outside the character loop (inside, the compiler if-converts it
         * into a 64-bit multiply on every character) */
        int first = 1;
        while (i < end) {
            int len = end - i < 9 ? end - i : 9, stop = i + len;
            unsigned w = 0;
            for (; i < stop; i++) {
                unsigned c = (unsigned)p[i] - '0';
                if (c > 9) {
                    unsigned char ch = p[i];
                    if (ch >= 'p' && ch <= 'y') { c = ch - 'p'; neg = 1; }     /* overpunch: last digit, or first with SIGN LEADING */
                    else if (ch == ' ') c = 0;              /* a space counts as zero */
                    /* Anything else: the low nibble.  This comment used to
                     * say "GnuCOBOL: low nibble", asserting a parity we do
                     * not have -- measured 2026-09-02, GnuCOBOL substitutes
                     * a zero digit here, so '001B' reads 10 there and 12
                     * here, and '001{' reads 10 there and 21 here.
                     *
                     * Neither is the EBCDIC overpunch, which is what a file
                     * converted from a mainframe carries: zone C over a
                     * digit is +0..+9 ({ABCDEFGHI), zone D is -0..-9
                     * (}JKLMNOPQR).  The low nibble happens to give the
                     * right digit for A..I and nothing else -- for { } and
                     * J..R it yields 10..13, impossible digit values that
                     * then corrupt the whole accumulated number through
                     * w = w * 10 + c, and it drops the sign of J..R
                     * entirely.  The ASCII overpunch is not standardised;
                     * p..y for -0..-9 is GnuCOBOL's and Micro Focus's
                     * choice and is the one we write, so nothing this
                     * toolchain produces reaches this line.  Reading
                     * EBCDIC-derived data is not a requirement anyone has
                     * asked for; if it becomes one, this is the line, and
                     * it needs the zone, not the nibble. */
                    else c = ch & 15;
                }
                w = w * 10 + c;
            }
            v = first ? (long long)w : v * pow10tab[len] + w; first = 0;
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
    /* the digit positions that hold a character: P scaling positions do not */
    int eff = d->digits;
    if (d->pic) for (const char *q = d->pic; *q; q++) if (*q == 'P') eff--;
    if (vscale > d->scale) {
        int m = vscale - d->scale;
        long long k = pow10tab[m], r;
        long long q = div_pow10(v, m, &r);
        if ((opts & 1) && (r < 0 ? -r : r) * 2 >= k) q += (v < 0) ? -1 : 1;
        v = q;
    } else if (vscale < d->scale) {
        /* scaling up can pass 64 bits (12345 into 9V9(17)): the size error
         * is decided on the integer digits first, then the digits that
         * cannot survive the receiver's width are dropped before the shift */
        int k = d->scale - vscale;
        if (!(d->flags & COB_F_NOTRUNC) && d->digits <= 18) {
            unsigned long long a = v < 0 ? (unsigned long long)(-v) : (unsigned long long)v;
            int lim = eff - d->scale + vscale;           /* integer positions, in v's scale */
            if (opts & 2) {
                if (lim < 0 ? a != 0 : (lim <= 18 && a >= (unsigned long long)pow10tab[lim])) return 1;
            }
            int keep = eff - k;
            if (keep <= 0) v = 0; else if (keep <= 18 && a >= (unsigned long long)pow10tab[keep]) div_pow10(v, keep, &v);
        }
        if (v) v *= pow10tab[k > 18 ? 18 : k];
    }

    int neg = v < 0;
    unsigned long long mag = neg ? (unsigned long long)(-v) : (unsigned long long)v;
    if (d->flags & COB_F_NOTRUNC) {
        if ((opts & 2) && d->size < 8) {
            unsigned long long lim = 1ULL << (d->size * 8 - ((d->flags & COB_F_SIGNED) ? 1 : 0));
            if (mag >= lim) return 1;
        }
    } else if (d->digits <= 18) {
        if (mag >= (unsigned long long)pow10tab[eff]) {
            if (opts & 2) return 1;
            udiv_pow10(mag, eff, &mag);
        }
    }
    if (!(d->flags & COB_F_SIGNED)) neg = 0;         /* unsigned takes the magnitude */

    if (d->cat == COB_NUM_ED) {
        char digs[40];
        int nd = d->digits;                         /* the P positions hold no digit character */
        for (const char *q = d->pic; q && *q; q++) if (*q == 'P') nd--;
        mag_to_digits(mag, digs, nd);
        int w = cob_edit_apply(d->pic, digs, neg, d->flags & COB_F_BLANKZ, (char *)p);
        if (cob_dp_comma) for (int i = 0; i < w; i++) { if (p[i] == '.') p[i] = ','; else if (p[i] == ',') p[i] = '.'; }
        if (cob_currency != '$') for (int i = 0; i < w; i++) if (p[i] == '$') p[i] = (unsigned char)cob_currency;
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
        mag_to_digits(mag, dg, digits);
        /* the last digit shares its byte with the sign; the rest pair off
         * leftwards, a zero nibble at the front when the count is even */
        int k = digits - 1, j = bytes - 1;
        p[j] = (unsigned char)(((dg[k] - '0') << 4) | ((d->flags & COB_F_SIGNED) ? (neg ? 0xD : 0xC) : 0xF));
        for (k--; k >= 0; k -= 2) {
            unsigned hi = k > 0 ? (unsigned)(dg[k - 1] - '0') : 0u;
            p[--j] = (unsigned char)((hi << 4) | (unsigned)(dg[k] - '0'));
        }
        while (j > 0) p[--j] = 0;
        break;
    }
    default: {
        int n = (int)d->size, i = n - 1, start = 0;
        if ((d->flags & COB_F_BLANKZ) && mag == 0) { memset(p, ' ', (size_t)n); break; }   /* BLANK WHEN ZERO on a plain numeric item */
        if (d->flags & COB_F_SEPLEAD) { p[0] = neg ? '-' : '+'; start = 1; }
        if (d->flags & COB_F_SEPTRAIL) { p[n - 1] = neg ? '-' : '+'; i = n - 2; }
        mag_to_digits(mag, (char *)p + start, i - start + 1);
        if (neg && !(d->flags & (COB_F_SEPLEAD | COB_F_SEPTRAIL))) {
            int k = (d->flags & COB_F_LEAD) ? 0 : n - 1;
            p[k] = (unsigned char)(p[k] - '0' + 'p');
        }
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
    if (is_signed) out_char(neg ? '-' : '+');
    mag_to_digits(mag, d, digits);
    for (int i = 0; i < digits; i++) {
        if (scale > 0 && i == digits - scale) out_char('.');
        out_char(d[i]);
    }
}

void cob_display_field(const void *vp, const cob_desc *d)
{
    const unsigned char *p = vp;
    if (d->cat != COB_NUM) { out_bytes((const char *)p, (int)d->size); return; }
    if (d->usage == COB_U_DISPLAY && !(d->flags & (COB_F_SEPLEAD | COB_F_SEPTRAIL)) && d->digits == d->size) {
        /* the digits as stored (a picture with P positions takes the general path) */
        int n = (int)d->size;
        int sk = (d->flags & COB_F_LEAD) ? 0 : n - 1;
        unsigned char last = p[sk];
        int neg = (d->flags & COB_F_SIGNED) && last >= 'p' && last <= 'y';
        if (d->flags & COB_F_SIGNED) out_char(neg ? '-' : '+');
        for (int i = 0; i < n; i++) {
            if (d->scale > 0 && i == n - d->scale) out_char(cob_dp_comma ? ',' : '.');
            unsigned char c = p[i];
            if (i == sk && neg) c = (unsigned char)(last - 'p' + '0');
            out_char((char)c);
        }
        return;
    }
    long long v = cob_get_num(p, d);
    int neg = v < 0;
    unsigned long long mag = neg ? (unsigned long long)(-v) : (unsigned long long)v;
    int digits = (d->flags & COB_F_NOTRUNC) ? capacity_digits(d->size) : d->digits;
    int scale = d->scale;
    if (scale < 0) { mag *= (unsigned long long)pow10tab[-scale]; scale = 0; }   /* trailing P: the value, its low zeros shown */
    emit_scaled(mag, neg, digits, scale, d->flags & COB_F_SIGNED);
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
/* the digits of a numeric item as an alphanumeric sending item: the
 * sign and the point unrepresented, and P positions as zeros (NC114M:
 * S9P(17) holding 1 reads 100000000000000000) */
static int num_to_digits(const void *p, const cob_desc *d, char *out)
{
    int np = 0, lead_p = 0;
    if (d->pic) { for (const char *q = d->pic; *q; q++) if (*q == 'P') np++; lead_p = np && (d->pic[0] == 'P' || (d->pic[0] == 'S' && d->pic[1] == 'P')); }
    if (d->usage == COB_U_DISPLAY && !(d->flags & (COB_F_SEPLEAD | COB_F_SEPTRAIL)) && !np) {
        memcpy(out, p, d->size);
        int sk = (d->flags & COB_F_LEAD) ? 0 : (int)d->size - 1;
        unsigned char last = (unsigned char)out[sk];
        if (last >= 'p' && last <= 'y') out[sk] = (char)(last - 'p' + '0');
        return (int)d->size;
    }
    long long v = cob_get_num(p, d);
    unsigned long long mag = v < 0 ? (unsigned long long)(-v) : (unsigned long long)v;
    int digits = (d->flags & COB_F_NOTRUNC) ? capacity_digits(d->size) : d->digits;
    if (np) {
        /* the stored digits, the P positions zeros on the side they sit */
        int stored = digits - np, o = 0;
        if (lead_p) for (int i = 0; i < np; i++) out[o++] = '0';
        mag_to_digits(mag, out + o, stored);
        o += stored;
        if (!lead_p) for (int i = 0; i < np; i++) out[o++] = '0';
        return o;
    }
    mag_to_digits(mag, out, digits);
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
        /* a numeric-edited sending item is alphanumeric to an alphanumeric
         * receiver: its edited characters as they are (NC124A's ZZZPP to X(5));
         * only a numeric one sends its digits */
        if (sd->cat != COB_NUM || dd->cat == COB_GROUP) {
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
    /* a run of digits longer than the receiver's integer positions: the
     * rightmost are the ones that survive (an unsigned integer moved to a
     * numeric item, X3.23 6.18.2; NC105A moves 28 digits into 9(10)) */
    unsigned run_end = i; while (run_end < n && s[run_end] >= '0' && s[run_end] <= '9') run_end++;
    if (run_end - i > 18 && (run_end == n || s[run_end] != '.')) i = run_end - 18;
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

/* PROGRAM COLLATING SEQUENCE: the rank of each character, or native order */
static const unsigned char *cob_collating;
const unsigned char *cob_set_collating(const unsigned char *t) { const unsigned char *old = cob_collating; cob_collating = t; return old; }

/* The program registry: every unit registers its PROGRAM-ID and entry
 * from .init_array before main; CALL identifier looks the name up. */
static struct { const char *name; void *fn; void (*cancel)(void); } cob_progs[256];
static int cob_nprogs;
void cob_register(const char *name, void *fn, void (*cancel)(void))
{
    if (cob_nprogs == 256) cob_fatal("more than 256 programs in one executable");
    cob_progs[cob_nprogs].name = name; cob_progs[cob_nprogs].fn = fn; cob_progs[cob_nprogs].cancel = cancel; cob_nprogs++;
}
static int prog_index(const unsigned char *p, int len)
{
    while (len > 0 && (p[len - 1] == ' ' || p[len - 1] == 0)) len--;
    for (int i = 0; i < cob_nprogs; i++) {
        const char *n = cob_progs[i].name; int k = 0;
        while (k < len && n[k] && tolower((unsigned char)n[k]) == tolower(p[k])) k++;
        if (k == len && !n[k]) return i;
    }
    return -1;
}
/* CANCEL: the program's WORKING-STORAGE back to its initial state; a
 * name that is not a program here is ignored, as GnuCOBOL does */
void cob_cancel(const unsigned char *p, int len)
{
    int i = prog_index(p, len);
    if (i >= 0 && cob_progs[i].cancel) cob_progs[i].cancel();
}
void *cob_resolve(const unsigned char *p, int len, int must)
{
    int i = prog_index(p, len);
    if (i >= 0) return cob_progs[i].fn;
    while (len > 0 && (p[len - 1] == ' ' || p[len - 1] == 0)) len--;
    if (must) {
        char msg[200]; snprintf(msg, sizeof msg, "CALL: the program '%.*s' is not in this executable", len > 120 ? 120 : len, (const char *)p);
        cob_fatal(msg);
    }
    return 0;
}

/* EXTERNAL: storage shared by name between the programs of one executable.
 * A record's block is made on first request (zeroed, as GnuCOBOL's); an
 * EXTERNAL file has one connector, the image of the first program to enter
 * with it, whose record area is that program's shared record block.  Each
 * program entering sets the connector's FILE STATUS item to its own and
 * puts the previous one back on exit, so the statement's own program's
 * status is the one written. */
static struct { const char *name; void *p; unsigned size; } cob_exts[128]; static int cob_nexts;
void *cob_external(const char *name, unsigned size)
{
    for (int i = 0; i < cob_nexts; i++)
        if (!strcmp(cob_exts[i].name, name)) {
            if (size > cob_exts[i].size) {
                void *q = calloc(size, 1);
                if (!q) cob_fatal("out of memory for an EXTERNAL item");
                memcpy(q, cob_exts[i].p, cob_exts[i].size);
                cob_exts[i].p = q; cob_exts[i].size = size;
            }
            return cob_exts[i].p;
        }
    if (cob_nexts == 128) cob_fatal("more than 128 EXTERNAL items");
    cob_exts[cob_nexts].name = name; cob_exts[cob_nexts].p = calloc(size ? size : 1, 1); cob_exts[cob_nexts].size = size;
    if (!cob_exts[cob_nexts].p) cob_fatal("out of memory for an EXTERNAL item");
    return cob_exts[cob_nexts++].p;
}
static struct { const char *name; cob_file *f; } cob_extf[64]; static int cob_nextf;
cob_file *cob_ext_file_enter(const char *name, cob_file *mine, void *rec)
{
    for (int i = 0; i < cob_nextf; i++)
        if (!strcmp(cob_extf[i].name, name)) {
            cob_file *f = cob_extf[i].f;
            if (f != mine) { mine->saved_status = f->status; f->status = mine->status; }
            return f;
        }
    if (cob_nextf == 64) cob_fatal("more than 64 EXTERNAL files");
    mine->record = rec;
    cob_extf[cob_nextf].name = name; cob_extf[cob_nextf].f = mine; cob_nextf++;
    return mine;
}
void cob_ext_file_exit(const char *name, cob_file *mine)
{
    for (int i = 0; i < cob_nextf; i++)
        if (!strcmp(cob_extf[i].name, name)) {
            cob_file *f = cob_extf[i].f;
            if (f != mine) f->status = mine->saved_status;
            return;
        }
}

/* CALL ... BY CONTENT: the callee is handed a copy, from an arena that
 * behaves as a stack -- pushed before the CALL, popped after it */
static char cob_content_arena[1 << 16]; static unsigned cob_content_top;
static unsigned cob_content_mark[256]; static int cob_content_nmark;
void *cob_content_push(const void *p, unsigned n)
{
    unsigned at = (cob_content_top + 7u) & ~7u;
    if (at + n > sizeof cob_content_arena || cob_content_nmark == 256) cob_fatal("BY CONTENT: too much or too deep");
    cob_content_mark[cob_content_nmark++] = cob_content_top;
    memcpy(cob_content_arena + at, p, n);
    cob_content_top = at + n;
    return cob_content_arena + at;
}
void cob_content_pop(int k)
{
    while (k-- > 0 && cob_content_nmark > 0) cob_content_top = cob_content_mark[--cob_content_nmark];
}

/* DECIMAL-POINT IS COMMA: the program's own; a called program's is restored on its exit */
int cob_dp_comma;
int cob_set_decimal_point(int comma) { int old = cob_dp_comma; cob_dp_comma = comma; return old; }
/* CURRENCY SIGN: the character printed where the picture says '$' */
int cob_currency = '$';
int cob_set_currency(int c) { int old = cob_currency; cob_currency = c ? c : '$'; return old; }

static int cmp_bytes(const unsigned char *a, int na, const unsigned char *b, int nb)
{
    int n = na > nb ? na : nb;          /* the shorter is extended with spaces */
    const unsigned char *t = cob_collating;
    for (int i = 0; i < n; i++) {
        int ca = i < na ? a[i] : ' ', cb = i < nb ? b[i] : ' ';
        if (t) { ca = t[ca]; cb = t[cb]; }
        if (ca != cb) return ca < cb ? -1 : 1;
    }
    return 0;
}

static unsigned long long cob_mag(long long v)
{
    return v < 0 ? 0 - (unsigned long long)v : (unsigned long long)v;
}

/* Can v be multiplied by 10^k without signed 64-bit overflow? */
static int cob_fits_mul10(long long v, int k)
{
    unsigned long long a;
    if (k <= 0) return 1;
    if (k > 18) return 0;
    a = cob_mag(v);
    if (a == 0) return 1;
    return a <= (unsigned long long)0x7fffffffffffffffLL / (unsigned long long)pow10tab[k];
}

static long long cob_rescale(long long v, int from, int to)
{
    int k;
    if (from == to) return v;
    if (from > to) {
        k = from - to;
        if (k > 18) return 0;
        return v / pow10tab[k];
    }
    k = to - from;
    if (!cob_fits_mul10(v, k))
        return v < 0 ? -0x7fffffffffffffffLL : 0x7fffffffffffffffLL;
    return v * pow10tab[k];
}

static int cmp_scaled(long long a, int sa, long long b, int sb)
{
    long long aa = cob_rescale(a, sa, sa > sb ? sa : sb);
    long long bb = cob_rescale(b, sb, sa > sb ? sa : sb);
    return aa < bb ? -1 : aa > bb ? 1 : 0;
}

/* -1, 0, 1 */
int cob_cmp(const void *a, const cob_desc *ad, const void *b, const cob_desc *bd)
{
    int an = ad->cat == COB_NUM, bn = bd->cat == COB_NUM;
    if (an && bn) return cmp_scaled(cob_get_num(a, ad), ad->scale, cob_get_num(b, bd), bd->scale);
    char ta[40], tb[40];
    const unsigned char *pa = a, *pb = b;
    int na = (int)ad->size, nb = (int)bd->size;
    /* one side numeric, the other not: the numeric side is compared as the
     * characters of its digits (a literal's sign, a separate sign, an
     * overpunch, a binary or packed usage all go) */
    unsigned signs = COB_F_SEPLEAD | COB_F_SEPTRAIL | COB_F_LEAD | COB_F_SIGNED;
    if (an && (ad->usage != COB_U_DISPLAY || (ad->flags & signs))) { na = num_to_digits(a, ad, ta); pa = (unsigned char *)ta; }
    if (bn && (bd->usage != COB_U_DISPLAY || (bd->flags & signs))) { nb = num_to_digits(b, bd, tb); pb = (unsigned char *)tb; }
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
            if (i == ((d->flags & COB_F_LEAD) ? 0 : n - 1) && (d->flags & COB_F_SIGNED) && c >= 'p' && c <= 'y') continue;
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

/* a SPECIAL-NAMES CLASS: every character of the item is in the class's
 * 256-entry table (the compiler builds it from the literals and ranges) */
int cob_class_user(const void *vp, const cob_desc *d, const unsigned char *tab)
{
    const unsigned char *p = vp;
    int n = (int)d->size;
    for (int i = 0; i < n; i++) if (!tab[p[i]]) return 0;
    return 1;
}

/* ---- the numeric stack: ADD/SUBTRACT/MULTIPLY/DIVIDE, COMPUTE later --- */

typedef struct { long long v; int scale; } cob_num;
static cob_num nstk[96];
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
    /* Prefer scaling the smaller magnitude up. If that would overflow
     * i64, shed a fraction digit from the other instead (GitHub #21). */
    while (a->scale != b->scale) {
        if (a->scale < b->scale) {
            if (cob_fits_mul10(a->v, 1)) { a->v *= 10; a->scale++; }
            else { b->v /= 10; b->scale--; }
        } else {
            if (cob_fits_mul10(b->v, 1)) { b->v *= 10; b->scale++; }
            else { a->v /= 10; a->scale--; }
        }
    }
}

static int div0;        /* a size error happened in this statement (div0 or overflow) */

void cob_nadd(void) { cob_num *a = &nstk[nsp - 2], *b = &nstk[nsp - 1]; align2(a, b); a->v += b->v; nsp--; }
void cob_nsub(void) { cob_num *a = &nstk[nsp - 2], *b = &nstk[nsp - 1]; align2(a, b); a->v -= b->v; nsp--; }
void cob_nmul(void)
{
    cob_num *a = &nstk[nsp - 2], *b = &nstk[nsp - 1];
    /* wide operands (the NIST suite works with S9V9(17) items): shed
     * fraction digits until the product fits 64 bits and 18 of scale */
    for (;;) {
        unsigned long long ua = a->v < 0 ? 0 - (unsigned long long)a->v : (unsigned long long)a->v;
        unsigned long long ub = b->v < 0 ? 0 - (unsigned long long)b->v : (unsigned long long)b->v;
        int over = ub && ua > 0x7fffffffffffffffULL / ub;
        if (!over && a->scale + b->scale <= 18) break;
        cob_num *w = a->scale >= b->scale ? a : b;
        if (w->scale == 0) w = w == a ? b : a;
        if (w->scale == 0) break;                    /* nothing left to shed: the store's size error catches it */
        w->v /= 10; w->scale--;
    }
    {
    unsigned long long ua = a->v < 0 ? 0 - (unsigned long long)a->v : (unsigned long long)a->v;
    unsigned long long ub = b->v < 0 ? 0 - (unsigned long long)b->v : (unsigned long long)b->v;
    if (ub && ua > 0x7fffffffffffffffULL / ub) {
        div0 = 1;                                   /* size error: product does not fit i64 */
        nsp--;
        return;
    }
    }
    a->v *= b->v; a->scale += b->scale; nsp--;
}

/* Division carries the operands' larger scale plus guard digits, so a
 * receiver with a wider scale than either operand still gets its digits;
 * the store truncates.  (The 85 intermediate rules are implementor-defined;
 * this is the stage-2 rule and stage 3 may tighten it.) */
void cob_ndiv(void)
{
    cob_num *a = &nstk[nsp - 2], *b = &nstk[nsp - 1];
    if (b->v == 0) { div0 = 1; nsp--; return; }   /* size error; the left operand stands in */
    /* long division in decimal: the integer quotient of the scaled values,
     * then one fraction digit at a time from the remainder (ten times a
     * remainder below 10^18 fits in 64 bits), until the quotient holds
     * seventeen digits or the wanted scale -- the operands' larger one plus
     * six guard digits -- is reached.  Exact, so 111111111.111111111
     * divided by itself is 1.000000000 and not 1.001001001. */
    int neg = (a->v < 0) != (b->v < 0);
    unsigned long long ua = a->v < 0 ? (unsigned long long)(-a->v) : (unsigned long long)a->v;
    unsigned long long ub = b->v < 0 ? (unsigned long long)(-b->v) : (unsigned long long)b->v;
    unsigned long long q, r;
    if (!((ua | ub) >> 32)) { unsigned x = (unsigned)ua, y = (unsigned)ub, qq = x / y; q = qq; r = x - qq * y; }
    else { q = ua / ub; r = ua % ub; }
    int scale = a->scale - b->scale;
    int want = (a->scale > b->scale ? a->scale : b->scale) + 6;
    if (want < 9) want = 9;                         /* an intrinsic's argument keeps nine fraction digits (TAN(1 / 180)) */
    if (want > 18) want = 18;                       /* the stack's scale never passes 18 */
    if (ub < (1u << 28)) {                          /* r * 10 stays in 32 bits: the hardware divider */
        unsigned d32 = (unsigned)ub, r32 = (unsigned)r;
        while (scale < want && q < (unsigned long long)pow10tab[17]) {
            r32 *= 10;
            unsigned t = r32 / d32;
            q = q * 10 + t; r32 -= t * d32;
            scale++;
        }
        r = r32;
    } else {
        while (scale < want && q < (unsigned long long)pow10tab[17]) {
            r *= 10;
            q = q * 10 + r / ub; r %= ub;
            scale++;
        }
    }
    if (scale < 0) {                            /* the divisor's scale exceeded the dividend's */
        while (scale < 0 && q < (unsigned long long)pow10tab[17]) { q *= 10; scale++; }
        if (scale < 0) { q = (unsigned long long)pow10tab[18]; scale = 0; }   /* beyond eighteen digits: a size error at the store */
    }
    a->v = neg ? -(long long)q : (long long)q; a->scale = scale;
    nsp--;
}

void cob_nneg(void) { nstk[nsp - 1].v = -nstk[nsp - 1].v; }
/* the top truncated to `scale` decimals (DIVIDE ... REMAINDER: the
 * quotient as it would be stored before ROUNDED, X3.23 6.9.4) */
void cob_ntrunc(int scale)
{
    cob_num *a = &nstk[nsp - 1];
    if (a->scale > scale) { a->v = div_pow10(a->v, a->scale - scale, 0); a->scale = scale; }
}

/* a ** b for an integer b >= 0; anything else is beyond this stage */
void cob_npow(void)
{
    cob_num *a = &nstk[nsp - 2], *b = &nstk[nsp - 1];
    if (b->scale > 0) { long long k = pow10tab[b->scale]; if (b->v % k) cob_fatal("** with a non-integer exponent is not implemented"); b->v /= k; b->scale = 0; }
    if (b->v < 0) cob_fatal("** with a negative exponent is not implemented");
    long long base = a->v, r = 1; int bs = a->scale, scale = 0;
    while (bs > 9) { base /= 10; bs--; }            /* nine fraction digits of base carry the answer */
    unsigned long long ab = base < 0 ? 0 - (unsigned long long)base : (unsigned long long)base;
    for (long long i = 0; i < b->v; i++) {
        while (scale > 0 && ab &&
               (unsigned long long)(r < 0 ? -r : r) > 0x7fffffffffffffffULL / ab) { r /= 10; scale--; }
        r *= base; scale += bs;
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
    if (d->scale > 0) v = div_pow10(v, d->scale, 0);
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
/* DECLARATIVES: the USE AFTER ERROR PROCEDURE section that applies to a
 * file -- its own, else the one for its open mode -- as a paragraph id
 * the compiled code dispatches on; 0 when none */
/* the open mode a USE ... ON INPUT/OUTPUT/I-O/EXTEND procedure is chosen by:
 * the mode the file is open in, or the one the failing OPEN tried */
int cob_open_mode(cob_file *f) { return f->open_mode ? (int)f->open_mode : (int)f->open_try; }

/* An I/O statement's result for the compiler's dispatch: 0 fine, 1 the
 * statement's own condition (AT END, INVALID KEY), 2 an error recorded in
 * a FILE STATUS, 3 an error with no FILE STATUS to record it -- the
 * compiler runs a USE procedure if one applies, else cob_io_unhandled. */
static char cob_last_st[3]; static const char *cob_last_op = "";
static int file_result(cob_file *f, const char *st, const char *what)
{
    set_status(f, st);
    if (st[0] == '0') return 0;
    if (st[0] == '1' || st[0] == '2') return 1;      /* at end; the invalid key condition */
    cob_last_st[0] = st[0]; cob_last_st[1] = st[1]; cob_last_op = what;
    return f->status ? 2 : 3;
}
void cob_io_unhandled(cob_file *f)
{
    (void)f;
    char msg[96];
    int n = 0;
    const char *pre = "file error (status ", *what = cob_last_op;
    while (*pre) msg[n++] = *pre++;
    msg[n++] = cob_last_st[0]; msg[n++] = cob_last_st[1]; msg[n++] = ')'; msg[n++] = ' ';
    while (*what && n < 90) msg[n++] = *what++;
    msg[n] = 0;
    cob_fatal(msg);
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
static int rel_read_next(cob_file *f);
static int rel_read_key(cob_file *f);
static int rel_write(cob_file *f, int reclen);
static int rel_rewrite(cob_file *f);
static int rel_delete(cob_file *f);
static int rel_start(cob_file *f, int op);
static unsigned rel_slot_size(cob_file *f);
static void lin_values(cob_file *f);

int cob_open(cob_file *f, int mode)
{
    int reversed = mode & 8; mode &= 7;
    f->reversed = 0;
    if (reversed) {
        if (f->org != COB_ORG_SEQ || f->varying) cob_fatal("OPEN REVERSED needs a sequential file of fixed-length records");
        int rc = cob_open(f, mode);
        if (rc == 0 && f->fp) {
            fseek((FILE *)f->fp, 0, 2);
            long end = ftell((FILE *)f->fp);
            f->fpos = end < 0 ? 0 : (unsigned)end; f->reversed = 1;
        }
        return rc;
    }
    f->open_try = (unsigned)mode;
    if (f->open_mode) return file_result(f, "41", "OPEN of a file already open");
    if (f->org == COB_ORG_INDEXED) return idx_open(f, mode);
    const char *name = file_name(f);
    const char *fm = mode == COB_OPEN_INPUT ? "rb" : mode == COB_OPEN_OUTPUT ? "wb"
                   : mode == COB_OPEN_EXTEND ? "ab" : "r+b";
    /* a relative file is addressed by slot, so EXTEND keeps read-write
     * access and positions after the last slot (created when absent) */
    if (f->org == COB_ORG_RELATIVE && mode == COB_OPEN_EXTEND) fm = "r+b";
    if (f->org == COB_ORG_RELATIVE && mode == COB_OPEN_OUTPUT) fm = "w+b";   /* WRITE checks the slot first */
    if (f->locked) return file_result(f, "38", "OPEN of a file closed WITH LOCK");
    FILE *fp;
    if (mode == COB_OPEN_EXTEND) {           /* "ab" would create it: look first */
        fp = fopen(name, "rb");
        if (fp) { fclose(fp); fp = fopen(name, fm); } else fp = 0;
    } else fp = fopen(name, fm);
    /* EXTEND or I-O on an absent file: an OPTIONAL one comes into being
     * (05), any other is 35 */
    if (!fp && (mode == COB_OPEN_EXTEND || mode == COB_OPEN_IO)) {
        if (!f->optional) return file_result(f, "35", name);
        fp = fopen(name, "w+b");
        if (fp) {
            f->fp = fp; f->open_mode = (unsigned char)mode; f->at_eof = 0; f->eof_seen = 0; f->last_len = 0; f->fpos = 0;
            if (f->org == COB_ORG_RELATIVE) { f->rel_pos = 1; f->rel_last = 0; }
            return file_result(f, "05", name);
        }
    }
    f->at_eof = 0; f->eof_seen = 0; f->last_len = 0; f->fpos = 0;
    f->rpos = f->rlen = 0;
    if (fp && f->org == COB_ORG_RELATIVE) {
        f->rel_pos = 1; f->rel_last = 0;
        if (mode == COB_OPEN_EXTEND && fseek(fp, 0, 2) == 0)
            f->rel_pos = (unsigned)(ftell(fp) / (long)rel_slot_size(f)) + 1;
    }
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
    if (mode == COB_OPEN_EXTEND && fseek(fp, 0, 2) == 0) { long e = ftell(fp); f->fpos = e > 0 ? (unsigned)e : 0; }
    if (f->linage) { lin_values(f); f->lin_counter = 1; f->lin_needs_top = 1; f->lin_eop = 0; }
    return file_result(f, "00", name);
}

int cob_close(cob_file *f);

/* CLOSE ... REEL/UNIT on a file that has no reels: 07 */
int cob_close_reel(cob_file *f)
{
    if (!f->open_mode) return file_result(f, "42", "CLOSE of a file not open");
    return file_result(f, "07", "");
}

/* CLOSE ... WITH LOCK: the file cannot be opened again in this run */
int cob_close_lock(cob_file *f)
{
    int r = cob_close(f);
    if (r == 0) f->locked = 1;
    return r;
}

int cob_close(cob_file *f)
{
    if (!f->open_mode) return file_result(f, "42", "CLOSE of a file not open");
    if (f->org == COB_ORG_INDEXED) return idx_close(f);
    if (f->fp) fclose((FILE *)f->fp);
    f->fp = 0; f->open_mode = 0; f->at_eof = 0;
    if (f->rbuf) { free(f->rbuf); f->rbuf = 0; }
    f->rpos = f->rlen = 0;
    return file_result(f, "00", "");
}

int cob_read(cob_file *f)
{
    if (!f->open_mode) return file_result(f, "47", "READ of a file not open");
    if (f->open_mode == COB_OPEN_OUTPUT || f->open_mode == COB_OPEN_EXTEND)
        return file_result(f, "47", "READ of a file open for output");
    if (f->org == COB_ORG_INDEXED) return idx_read_next(f);
    if (f->org == COB_ORG_RELATIVE) return rel_read_next(f);
    if (f->at_eof) {
        /* the end: 10 the first time, 46 for every READ after it */
        f->last_len = 0;
        if (f->eof_seen) return file_result(f, "46", "");
        f->eof_seen = 1;
        return file_result(f, "10", "");
    }
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
        if (got == 0) { f->at_eof = 1; f->eof_seen = 1; f->last_len = 0; return file_result(f, "10", ""); }
        if (got < 4) return file_result(f, "30", "truncated RDW");
        unsigned len = ((unsigned)rdw[0] << 8) | rdw[1];
        if (len < 4) return file_result(f, "30", "bad RDW");
        len -= 4;
        unsigned take = len < n ? len : n;
        if (fread(rec, 1, take, fp) != take) return file_result(f, "30", "truncated record");
        f->fpos += 4 + len;
        if (len > n) { fseek(fp, (long)f->fpos, 0); }
        f->last_len = take;
        if (f->dep_item) cob_put_num(f->dep_item, (const cob_desc *)f->dep_desc, (long long)take, 0);
        return file_result(f, len > n ? "04" : "00", "");
    }
    if (f->org == COB_ORG_SEQ && f->reversed) {
        /* REVERSED: the record before the position, the position moved back */
        if (f->fpos < n) { f->at_eof = 1; f->eof_seen = 1; f->last_len = 0; return file_result(f, "10", ""); }
        f->fpos -= n;
        fseek(fp, (long)f->fpos, 0);
        if (fread(rec, 1, n, fp) != n) return file_result(f, "30", "read");
        f->last_len = n;
        return file_result(f, "00", "");
    }
    if (f->org == COB_ORG_SEQ) {
        size_t got = fread(rec, 1, n, fp);
        if (got == 0) { f->at_eof = 1; f->eof_seen = 1; f->last_len = 0; return file_result(f, "10", ""); }
        f->fpos += (unsigned)got;
        if (got < n) { memset(rec + got, ' ', n - got); f->last_len = (unsigned)got; return file_result(f, "04", ""); }
        f->last_len = n;
        return file_result(f, "00", "");
    }

    /* line sequential: the bytes to the newline, out of a block buffer of
     * the runtime's own -- a byte through fgetc is thirty-odd instructions
     * on this target, a memchr over a block about three */
    if (!f->rbuf) { f->rbuf = malloc(COB_RBUF); f->rpos = f->rlen = 0; if (!f->rbuf) cob_fatal("out of memory"); }
    unsigned i = 0; int truncated = 0, any = 0;
    for (;;) {
        if (f->rpos >= f->rlen) {
            size_t got = fread(f->rbuf, 1, COB_RBUF, fp);
            if (got == 0) break;
            f->rpos = 0; f->rlen = (unsigned)got;
        }
        any = 1;
        unsigned char *s = (unsigned char *)f->rbuf + f->rpos, *e = memchr(s, '\n', f->rlen - f->rpos);
        unsigned take = e ? (unsigned)(e - s) : f->rlen - f->rpos;
        if (take) {
            if (i < n) { unsigned c = take < n - i ? take : n - i; memcpy(rec + i, s, c); i += c; if (c < take) truncated = 1; }
            else truncated = 1;
        }
        f->rpos += take + (e ? 1 : 0);
        f->fpos += take + (e ? 1 : 0);
        if (e) break;
    }
    if (!any) { f->at_eof = 1; f->eof_seen = 1; f->last_len = 0; return file_result(f, "10", ""); }
    if (i > 0 && rec[i - 1] == '\r') i--;
    f->last_len = i;
    if (i < n) memset(rec + i, ' ', n - i);
    return file_result(f, truncated ? "04" : "00", "");
}

/* ---- LINAGE: the logical page of a print file ------------------------- */
/* The page is lin_top blank lines, lin_lines lines the records go on, and
 * lin_bot blank lines; LINAGE-COUNTER is the line the device stands on.
 * The steps below are GnuCOBOL's (fileio.c, cob_linage_write_opt), so the
 * bytes and the counter agree with the oracle: a WRITE AFTER n LINES adds
 * n to the counter and n-1 blank lines before the record; past the last
 * line it fills the page, the bottom and the next top and starts at 1;
 * END-OF-PAGE is the footing reached or the page overflowed. */

static unsigned lin_value(cob_file *f, int which)
{
    const unsigned *t = (const unsigned *)f->linage + which * 3;
    if (t[1]) { long long v = cob_get_num((const void *)(size_t)t[1], (const cob_desc *)(size_t)t[2]); return v > 0 ? (unsigned)v : 0; }
    return t[0];
}

static void lin_values(cob_file *f)
{
    f->lin_lines = lin_value(f, 0); f->lin_foot = lin_value(f, 1); f->lin_top = lin_value(f, 2); f->lin_bot = lin_value(f, 3);
    if (f->lin_lines < 1) f->lin_lines = 1;
    if (f->lin_foot > f->lin_lines) f->lin_foot = 0;
}

static void lin_newlines(cob_file *f, unsigned n) { FILE *fp = (FILE *)f->fp; for (unsigned i = 0; i < n; i++) fputc('\n', fp); f->fpos += n; }

static void lin_new_page(cob_file *f, unsigned from)
{
    if (from < f->lin_lines) lin_newlines(f, f->lin_lines - from);
    lin_newlines(f, f->lin_bot);
    lin_values(f);
    lin_newlines(f, f->lin_top);
    f->lin_counter = 1;
}

/* ADVANCING n LINES on a LINAGE file, before or after the record */
static void lin_lines_opt(cob_file *f, unsigned n)
{
    unsigned was = f->lin_counter;
    f->lin_counter += n;
    if (f->lin_foot && f->lin_counter >= f->lin_foot) f->lin_eop = 1;
    if (f->lin_counter > f->lin_lines) { f->lin_eop = 1; lin_new_page(f, was); }
    else if (n > 1) lin_newlines(f, n - 1);
}

static int lin_write(cob_file *f, int before, int after)
{
    FILE *fp = (FILE *)f->fp;
    const char *rec = f->record;
    unsigned n = f->recsize;
    f->lin_eop = 0;
    if (f->lin_needs_top) { lin_newlines(f, f->lin_top); f->lin_needs_top = 0; }
    if (before == 0 && after == 0) after = 1;               /* no ADVANCING phrase: BEFORE ADVANCING 1 */
    if (before < 0) lin_new_page(f, f->lin_counter);       /* AFTER ADVANCING PAGE */
    else if (before > 0) lin_lines_opt(f, (unsigned)before);
    /* the whole record, trailing spaces included (GnuCOBOL keeps them on a LINAGE file) */
    if (fwrite(rec, 1, n, fp) != n) return file_result(f, "30", "write failed");
    fputc('\n', fp); f->fpos += n + 1;
    if (after < 0) lin_new_page(f, f->lin_counter);        /* BEFORE ADVANCING PAGE */
    else if (after > 0) lin_lines_opt(f, (unsigned)after);
    f->last_len = 0;
    return file_result(f, "00", "");
}

/* before/after: extra newlines around the record (ADVANCING); reclen:
 * the size of the 01 the WRITE named, which is the length of a mode-V
 * record unless DEPENDING ON says otherwise */
int cob_write(cob_file *f, int before, int after, int reclen)
{
    if (!f->open_mode) return file_result(f, "48", "WRITE of a file not open");
    if (f->open_mode == COB_OPEN_INPUT) return file_result(f, "48", "WRITE of a file open for input");
    if (f->open_mode == COB_OPEN_IO && (f->org == COB_ORG_SEQ || f->org == COB_ORG_LINESEQ))
        return file_result(f, "48", "WRITE of a sequential file open I-O");
    if (f->org == COB_ORG_INDEXED) return idx_write(f);
    if (f->org == COB_ORG_RELATIVE) return rel_write(f, reclen);
    if (f->linage) return lin_write(f, before, after);
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
        f->fpos += 4 + len; f->last_len = 0;
        return file_result(f, "00", "");
    }
    if (f->org == COB_ORG_SEQ) {
        if (fwrite(rec, 1, n, fp) != n) return file_result(f, "30", "write failed");
        f->fpos += n; f->last_len = 0;
        return file_result(f, "00", "");
    }
    /* ADVANCING: n extra newlines before or after; -1 is PAGE, a form feed;
     * -2 is ZERO LINES -- BEFORE ADVANCING ZERO leaves the record without
     * its newline, the next record's AFTER supplying one (SQ101M) */
    if (f->nl_pending) { fputc('\n', fp); f->fpos++; f->nl_pending = 0; }   /* the line the last record left open */
    if (before == -1) { fputc('\f', fp); f->fpos++; }
    for (int i = 0; i < before; i++) { fputc('\n', fp); f->fpos++; }
    while (n > 0 && rec[n - 1] == ' ') n--;
    if (n && fwrite(rec, 1, n, fp) != n) return file_result(f, "30", "write failed");
    f->fpos += n;
    if (after != -2) { fputc('\n', fp); f->fpos++; } else f->nl_pending = 1;
    if (after == -1) { fputc('\f', fp); f->fpos++; }
    for (int i = 0; i < after; i++) { fputc('\n', fp); f->fpos++; }
    f->last_len = 0;
    return file_result(f, "00", "");
}

/* ====================================================================== */
/* Relative I-O.  The file is fixed slots of 4 + recsize bytes; record n  */
/* is slot n.  A slot's four-byte prefix is the RDW our mode-V files      */
/* carry (big-endian length including the four, then two zero bytes),    */
/* all zero for an empty slot -- so a relative file is a sequence of     */
/* fixed-length V records, and a deleted record is unambiguous.          */
/* (GnuCOBOL 4 keeps an 8-byte native length there; docs/oracles.md.)    */
/* ====================================================================== */

static unsigned rel_slot_size(cob_file *f) { return 4 + f->recsize; }

static long rel_key_value(cob_file *f)
{
    if (!f->rel_key) return 0;
    return (long)cob_get_num(f->rel_key, (const cob_desc *)f->rel_key_desc);
}

static void rel_key_set(cob_file *f, unsigned n)
{
    if (f->rel_key) cob_put_num(f->rel_key, (const cob_desc *)f->rel_key_desc, (long long)n, 0);
}

/* the slot's state: 1 holds a record (read into the area when into_area),
 * 0 empty, -1 beyond the end of the file, -2 an I/O error */
static int rel_slot_get(cob_file *f, unsigned n, int into_area)
{
    FILE *fp = (FILE *)f->fp;
    unsigned char rdw[4];
    if (n < 1) return -1;
    if (fseek(fp, (long)(n - 1) * (long)rel_slot_size(f), 0) != 0) return -2;
    size_t got = fread(rdw, 1, 4, fp);
    if (got == 0) return -1;
    if (got < 4) return -2;
    unsigned len = ((unsigned)rdw[0] << 8) | rdw[1];
    if (len == 0) return 0;
    if (!into_area) return 1;
    if (len < 4) return -2;
    len -= 4;
    if (len > f->recsize) len = f->recsize;
    if (fread(f->record, 1, len, fp) != len) return -2;
    f->last_len = len;
    if (f->dep_item) cob_put_num(f->dep_item, (const cob_desc *)f->dep_desc, (long long)len, 0);
    return 1;
}

/* write slot n from the record area -- len bytes of it, the RDW saying so,
 * the rest of the slot zero -- or mark it empty; slots between the end of
 * the file and n come into being empty */
static int rel_slot_put(cob_file *f, unsigned n, int empty, unsigned len)
{
    FILE *fp = (FILE *)f->fp;
    unsigned sz = rel_slot_size(f);
    static const unsigned char zero[64];
    if (fseek(fp, 0, 2) != 0) return 0;
    long end = ftell(fp), want = (long)(n - 1) * (long)sz;
    for (long left = want - end; left > 0; ) {
        size_t k = left > 64 ? 64 : (size_t)left;
        if (fwrite(zero, 1, k, fp) != k) return 0;
        left -= (long)k;
    }
    if (fseek(fp, want, 0) != 0) return 0;
    unsigned char rdw[4] = { 0, 0, 0, 0 };
    if (empty) len = 0;
    if (len > f->recsize) len = f->recsize;
    if (!empty) { rdw[0] = (unsigned char)((len + 4) >> 8); rdw[1] = (unsigned char)((len + 4) & 255); }
    if (fwrite(rdw, 1, 4, fp) != 4) return 0;
    if (len && fwrite(f->record, 1, len, fp) != len) return 0;
    for (unsigned left = f->recsize - len; left > 0; ) {
        size_t k = left > 64 ? 64 : left;
        if (fwrite(zero, 1, k, fp) != k) return 0;
        left -= (unsigned)k;
    }
    fflush(fp);
    return 1;
}

static unsigned rel_slot_count(cob_file *f)
{
    FILE *fp = (FILE *)f->fp;
    if (fseek(fp, 0, 2) != 0) return 0;
    return (unsigned)(ftell(fp) / (long)rel_slot_size(f));
}

/* READ [NEXT]: the next slot that holds a record; the key item learns its number */
static int rel_read_next(cob_file *f)
{
    if (f->at_eof) {
        if (f->eof_seen) return file_result(f, "46", "");
        f->eof_seen = 1; return file_result(f, "10", "");
    }
    for (unsigned n = f->rel_pos; ; n++) {
        int r = rel_slot_get(f, n, 1);
        if (r == -2) return file_result(f, "30", "read failed");
        if (r == -1) { f->at_eof = 1; f->eof_seen = 1; f->rel_last = 0; return file_result(f, "10", ""); }
        if (r == 0) continue;
        if (f->rel_key) {       /* a record number the RELATIVE KEY item cannot hold: 14 */
            int kd = ((const cob_desc *)f->rel_key_desc)->digits;
            if (kd > 0 && kd < 10 && (long long)n >= pow10tab[kd]) { f->rel_pos = n + 1; return file_result(f, "14", ""); }
        }
        f->rel_last = n; f->rel_pos = n + 1;
        rel_key_set(f, n);
        return file_result(f, "00", "");
    }
}

/* READ (random): the record the RELATIVE KEY names */
static int rel_read_key(cob_file *f)
{
    long k = rel_key_value(f);
    if (k < 1) { f->rel_last = 0; return file_result(f, "23", ""); }
    int r = rel_slot_get(f, (unsigned)k, 1);
    if (r == -2) return file_result(f, "30", "read failed");
    if (r <= 0) { f->rel_last = 0; return file_result(f, "23", ""); }
    f->rel_last = (unsigned)k; f->rel_pos = (unsigned)k + 1; f->at_eof = 0;
    return file_result(f, "00", "");
}

/* the length a WRITE records: DEPENDING ON's value, else the 01 named,
 * else the record area (a relative file may hold variable-length records;
 * the slot stays the maximum) */
static int rel_write_len(cob_file *f, int reclen, unsigned *len)
{
    *len = reclen > 0 ? (unsigned)reclen : f->recsize;
    if (f->dep_item) {
        long long d = cob_get_num(f->dep_item, (const cob_desc *)f->dep_desc);
        if (d < (long long)f->minlen || d > (long long)f->recsize) return file_result(f, "44", "record length outside RECORD VARYING bounds");
        *len = (unsigned)d;
    }
    return 0;
}

/* WRITE: sequential access fills the next slot and tells the key item;
 * random access takes the slot the key names -- occupied is 22, 0 is 24 */
static int rel_write(cob_file *f, int reclen)
{
    unsigned n, len; int rc;
    if ((rc = rel_write_len(f, reclen, &len))) return rc;
    if (f->access == 0) n = f->rel_pos;
    else {
        long k = rel_key_value(f);
        if (k < 1) return file_result(f, "24", "");
        n = (unsigned)k;
        int r = rel_slot_get(f, n, 0);
        if (r == -2) return file_result(f, "30", "read failed");
        if (r == 1) return file_result(f, "22", "");
    }
    if (f->access == 0 && f->rel_key) {
        /* the record number must fit the RELATIVE KEY item */
        int kd = ((const cob_desc *)f->rel_key_desc)->digits;
        if (kd > 0 && kd < 10 && (long long)n >= pow10tab[kd]) return file_result(f, "14", "");
    }
    if (!rel_slot_put(f, n, 0, len)) return file_result(f, "30", "write failed");
    if (f->access == 0) { f->rel_pos = n + 1; rel_key_set(f, n); }
    return file_result(f, "00", "");
}

/* the slot REWRITE/DELETE act on: the last READ under sequential access
 * (43 when there was none), the key's under random or dynamic (23 absent) */
static int rel_target(cob_file *f, unsigned *n)
{
    if (f->access == 0) {
        if (!f->rel_last) return file_result(f, "43", "");
        *n = f->rel_last;
    } else {
        long k = rel_key_value(f);
        if (k < 1) return file_result(f, "23", "");
        *n = (unsigned)k;
    }
    int r = rel_slot_get(f, *n, 0);
    if (r == -2) return file_result(f, "30", "read failed");
    if (r <= 0) return file_result(f, "23", "");
    return 0;
}

static int rel_rewrite(cob_file *f)
{
    unsigned n, len; int rc = rel_target(f, &n);
    if (rc) return rc;
    if ((rc = rel_write_len(f, 0, &len))) return rc;
    if (!rel_slot_put(f, n, 0, len)) return file_result(f, "30", "write failed");
    return file_result(f, "00", "");
}

static int rel_delete(cob_file *f)
{
    unsigned n; int rc = rel_target(f, &n);
    if (rc) return rc;
    if (!rel_slot_put(f, n, 1, 0)) return file_result(f, "30", "write failed");
    f->rel_last = 0;
    return file_result(f, "00", "");
}

/* START: position on the first (or, for < and <=, the last) occupied slot
 * in the relation to the key; the key item is left alone */
static int rel_start(cob_file *f, int op)
{
    long k = rel_key_value(f);
    unsigned count = rel_slot_count(f), found = 0;
    if (op == 3 || op == 4) {
        long from = op == 3 ? k - 1 : k;
        if (from > (long)count) from = (long)count;
        for (long n = from; n >= 1 && !found; n--) if (rel_slot_get(f, (unsigned)n, 0) == 1) found = (unsigned)n;
    } else {
        long from = op == 1 ? k + 1 : k;
        if (from < 1) from = 1;
        if (op == 0) { if (k >= 1 && rel_slot_get(f, (unsigned)k, 0) == 1) found = (unsigned)k; }
        else for (long n = from; n <= (long)count && !found; n++) if (rel_slot_get(f, (unsigned)n, 0) == 1) found = (unsigned)n;
    }
    if (!found) return file_result(f, "23", "");
    f->rel_pos = found; f->rel_last = 0; f->at_eof = 0;
    return file_result(f, "00", "");
}

/* ====================================================================== */
/* SORT (Sort-Merge, the file form).  The SD is a cob_file of              */
/* organization SORT; while a SORT statement runs, its records live in     */
/* memory behind the SD's idx pointer.  USING reads a file through the    */
/* ordinary READ, GIVING writes through the ordinary WRITE, so the        */
/* input and output files keep their own organizations and framings.     */
/* The sort is a merge sort on an index array: stable, which is what     */
/* WITH DUPLICATES IN ORDER asks for and costs nothing to give always.   */
/* ====================================================================== */

typedef struct {
    const cob_sort_key *keys; int nkeys;
    const unsigned char *coll;      /* SORT ... COLLATING SEQUENCE: the alphabet's ranks, or 0 */
    unsigned klen;                  /* the normalized key (below) */
    unsigned char *kbuf;            /* one key, built at RELEASE */
    unsigned n;                     /* records released: the arrival number */
    xsort xs;                       /* the engine: entries of klen + recsize */
    int sorted;                     /* cob_sort_perform ran */
} cob_sorter;

/* The sort's memory: half the heap the program was linked with, or
 * S32_SORT_MEMORY (bytes, or with a K/M suffix) -- there is no sbrk,
 * so this is a share of a fixed pool, not a request for more.  The
 * fan-in is S32_SORT_FAN, default 32 (the emulator has 128 descriptors). */
extern char __heap_start[];
extern char __heap_end[];
static size_t sort_budget(void)
{
    const char *e = getenv("S32_SORT_MEMORY");
    if (e && *e) {
        char *end; unsigned long v = strtoul(e, &end, 10);
        if (*end == 'K' || *end == 'k') v <<= 10; else if (*end == 'M' || *end == 'm') v <<= 20;
        if (v >= 4096) return v;
    }
    size_t heap = (size_t)(__heap_end - __heap_start);
    return heap / 2;
}
static unsigned sort_fan(void)
{
    const char *e = getenv("S32_SORT_FAN");
    unsigned f = e && *e ? (unsigned)atoi(e) : 32;
    return f < 2 ? 2 : f > 100 ? 100 : f;
}

/* The normalized key: every SORT key rendered as bytes whose unsigned
 * byte order IS the COBOL order, so one memcmp compares a record on all
 * its keys at once (what DFSORT does; cob_cmp per key per comparison
 * was the sort's whole cost).  A numeric key becomes its cob_get_num
 * value as eight big-endian bytes with the sign bit flipped -- every
 * record of a key shares the descriptor, so the scale is common and
 * needs no alignment, and cob_cmp on two numerics is exactly this
 * order.  Any other key is its bytes through the collating sequence,
 * which is cmp_bytes on equal lengths.  A DESCENDING key's bytes are
 * complemented.  The arrival number trails every key, so equal keys
 * stay in RELEASE order: WITH DUPLICATES IN ORDER for free, and the
 * merge needs no tie-break of its own. */
static unsigned sort_klen(const cob_sorter *so)
{
    unsigned k = 4;
    for (int i = 0; i < so->nkeys; i++) {
        const cob_desc *d = so->keys[i].desc;
        k += d->cat == COB_NUM || d->cat == COB_NUM_ED ? 8 : d->size;
    }
    return k;
}

static void sort_key_build(const cob_sorter *so, const char *rec, unsigned seq, unsigned char *out)
{
    const unsigned char *t = so->coll ? so->coll : cob_collating;
    for (int i = 0; i < so->nkeys; i++) {
        const cob_sort_key *k = &so->keys[i];
        const cob_desc *d = k->desc;
        unsigned char *o = out;
        if (d->cat == COB_NUM || d->cat == COB_NUM_ED) {
            unsigned long long u = (unsigned long long)cob_get_num(rec + k->offset, d) ^ (1ULL << 63);
            for (int b = 7; b >= 0; b--) { o[b] = (unsigned char)u; u >>= 8; }
            out += 8;
        } else {
            const unsigned char *p = (const unsigned char *)rec + k->offset;
            unsigned n = d->size;
            if (t) for (unsigned b = 0; b < n; b++) o[b] = t[p[b]];
            else memcpy(o, p, n);
            out += n;
        }
        if (k->descending) for (unsigned char *q = o; q < out; q++) *q = (unsigned char)~*q;
    }
    out[0] = (unsigned char)(seq >> 24); out[1] = (unsigned char)(seq >> 16);
    out[2] = (unsigned char)(seq >> 8);  out[3] = (unsigned char)seq;
}

void cob_sort_begin(cob_file *sd, const cob_sort_key *keys, int nkeys, int dups, const unsigned char *coll)
{
    (void)dups;
    if (sd->org != COB_ORG_SORT) cob_fatal("SORT of a file that is not an SD");
    cob_sorter *so = calloc(1, sizeof *so);
    if (!so) cob_fatal("SORT: out of memory");
    so->keys = keys; so->nkeys = nkeys; so->coll = coll;
    so->klen = sort_klen(so);
    so->kbuf = malloc(so->klen);
    if (!so->kbuf) cob_fatal("SORT: out of memory");
    xs_init(&so->xs, so->klen + sd->recsize, so->klen, sort_budget(), sort_fan(), file_name(sd), cob_fatal);
    sd->idx = so; sd->open_mode = COB_OPEN_IO; sd->at_eof = 0;
}

static cob_sorter *sorter_of(cob_file *sd, const char *what)
{
    if (sd->org != COB_ORG_SORT || !sd->idx) { char m[80]; snprintf(m, sizeof m, "%s outside a SORT of its SD", what); cob_fatal(m); }
    return (cob_sorter *)sd->idx;
}

/* RELEASE: the SD's record area joins the set to be sorted */
void cob_release(cob_file *sd)
{
    cob_sorter *so = sorter_of(sd, "RELEASE");
    sort_key_build(so, sd->record, so->n, so->kbuf);
    xs_put(&so->xs, so->kbuf, sd->record);
    so->n++;
}

/* a record moves between a file's area and the SD's as a group MOVE would */
static void sort_copy(char *dst, unsigned dn, const char *src, unsigned sn)
{
    unsigned k = sn < dn ? sn : dn;
    memcpy(dst, src, k);
    if (k < dn) memset(dst + k, ' ', dn - k);
}

/* USING: every record of a file, read as that file reads */
void cob_sort_using(cob_file *sd, cob_file *in)
{
    cob_sorter *so = sorter_of(sd, "SORT USING");
    (void)so;
    if (cob_open(in, COB_OPEN_INPUT) == 2) cob_fatal("SORT USING: cannot open the input file");
    for (;;) {
        int r = cob_read(in);
        if (r == 1) break;
        if (r == 2) cob_fatal("SORT USING: read failed");
        sort_copy(sd->record, sd->recsize, in->record, in->last_len ? in->last_len : in->recsize);
        cob_release(sd);
    }
    cob_close(in);
}

/* MERGE USING: a file already in key order joins as presorted runs */
void cob_merge_using(cob_file *sd, cob_file *in)
{
    cob_sorter *so = sorter_of(sd, "MERGE USING");
    xs_source_begin(&so->xs);
    if (cob_open(in, COB_OPEN_INPUT) == 2) cob_fatal("MERGE USING: cannot open the input file");
    for (;;) {
        int r = cob_read(in);
        if (r == 1) break;
        if (r == 2) cob_fatal("MERGE USING: read failed");
        sort_copy(sd->record, sd->recsize, in->record, in->last_len ? in->last_len : in->recsize);
        cob_release(sd);
    }
    cob_close(in);
    xs_source_end(&so->xs);
}

void cob_sort_perform(cob_file *sd)
{
    cob_sorter *so = sorter_of(sd, "SORT");
    xs_finish(&so->xs);
    so->sorted = 1; sd->at_eof = 0;
}

/* GIVING: the sorted records, written as that file writes */
void cob_sort_giving(cob_file *sd, cob_file *out)
{
    cob_sorter *so = sorter_of(sd, "SORT GIVING");
    if (cob_open(out, COB_OPEN_OUTPUT) == 2) cob_fatal("SORT GIVING: cannot open the output file");
    const unsigned char *e;
    while ((e = xs_next(&so->xs)) != 0) {
        sort_copy(out->record, out->recsize, (const char *)e + so->klen, sd->recsize);
        if (cob_write(out, 0, 0, 0) == 2) cob_fatal("SORT GIVING: write failed");
    }
    cob_close(out);
}

/* RETURN: the next sorted record into the SD's area; 1 at end */
int cob_return(cob_file *sd)
{
    cob_sorter *so = sorter_of(sd, "RETURN");
    if (!so->sorted) cob_fatal("RETURN before the sort (RETURN belongs in the OUTPUT PROCEDURE)");
    const unsigned char *e = xs_next(&so->xs);
    if (!e) { sd->at_eof = 1; return file_result(sd, "10", ""); }
    memcpy(sd->record, e + so->klen, sd->recsize);
    sd->last_len = sd->recsize;
    return file_result(sd, "00", "");
}

void cob_sort_end(cob_file *sd)
{
    cob_sorter *so = sorter_of(sd, "SORT");
    xs_free(&so->xs); free(so->kbuf); free(so);
    sd->idx = 0; sd->open_mode = 0;
}

/* MOVE of a group whose last child is an OCCURS DEPENDING ON table: the
 * sending length is base + n occurrences of elem; the receiver's is what
 * it was laid out at (its maximum) */
void cob_move_odo(const void *src, void *dst, int n, int dstlen, int base, int elem)
{
    if (n < 0) n = 0;
    cob_move_alnum(src, base + n * elem, dst, dstlen, 0);
}

int cob_odo_length(int d, int base, int elem) { return d < 0 ? base : base + d * elem; }

/* SPECIAL-NAMES SWITCH-1..8: from the environment at start (cob_init), then SET */
int cob_switches[8];

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

/* ---- UNSTRING ---------------------------------------------------------
 * begin (source, its length, the POINTER or 0), the DELIMITED BY list,
 * then one call per receiver: the characters up to the leftmost
 * delimiter (the first listed wins at equal positions; ALL takes the
 * repeats too) go to the receiver by the MOVE rules, the delimiter to
 * DELIMITER IN, their count to COUNT IN.  Receivers left over when the
 * source is exhausted are untouched; source left over when the
 * receivers are is the overflow, as is a POINTER outside the source. */
static struct {
    const char *src; int slen, pos, overflow, tally, moved;
    struct { const char *p; int n, all; } d[16]; int nd;
} cu;

void cob_unstr_begin(const char *src, int slen, int pos)
{
    cu.src = src; cu.slen = slen; cu.overflow = 0; cu.tally = 0; cu.nd = 0; cu.moved = 0;
    cu.pos = pos ? pos : 1;
    if (cu.pos < 1 || cu.pos > slen) cu.overflow = 1;
}
void cob_unstr_setlen(int slen) { cu.slen = slen; if (cu.pos > slen) cu.overflow = 1; }
void cob_unstr_delim(const char *p, int n, int all)
{
    if (cu.nd == 16) cob_fatal("UNSTRING: more than 16 delimiters");
    cu.d[cu.nd].p = p; cu.d[cu.nd].n = n; cu.d[cu.nd].all = all; cu.nd++;
}
void cob_unstr_into(void *dst, const cob_desc *dd, void *ddst, const cob_desc *ddd, void *cdst, const cob_desc *cdd)
{
    if (cu.overflow || cu.pos > cu.slen) return;
    int start = cu.pos - 1, i = start, hit = -1;
    if (cu.nd == 0) {
        /* no DELIMITED BY: as many characters as the receiver holds (one
         * fewer for a separate sign) */
        int room = (int)dd->size - ((dd->flags & (COB_F_SEPLEAD | COB_F_SEPTRAIL)) ? 1 : 0);
        if (room < 0) room = 0;
        i = start + room; if (i > cu.slen) i = cu.slen;
    } else {
        for (; i < cu.slen && hit < 0; i++)
            for (int k = 0; k < cu.nd; k++)
                if (cu.d[k].n && i + cu.d[k].n <= cu.slen && !memcmp(cu.src + i, cu.d[k].p, cu.d[k].n)) { hit = k; break; }
        if (hit >= 0) i--;                      /* the delimiter's position */
    }
    int k = i - start;                          /* the examined characters */
    cob_desc sd; memset(&sd, 0, sizeof sd); sd.cat = COB_ALNUM; sd.size = (unsigned)k;
    if (k) cob_move(cu.src + start, &sd, dst, dd);
    else { sd.size = 1; cob_move(dd->cat == COB_NUM || dd->cat == COB_NUM_ED ? "0" : " ", &sd, dst, dd); }
    if (cdst) cob_put_num(cdst, cdd, k, 0);
    if (hit >= 0) {
        int dn = cu.d[hit].n;
        if (ddst) { sd.size = (unsigned)dn; cob_move(cu.d[hit].p, &sd, ddst, ddd); }
        i += dn;
        if (cu.d[hit].all) while (i + dn <= cu.slen && !memcmp(cu.src + i, cu.d[hit].p, dn)) i += dn;
    } else if (ddst) { sd.size = 1; cob_move(" ", &sd, ddst, ddd); }
    cu.pos = i + 1;
    cu.tally++; cu.moved = 1;
}
int cob_unstr_pointer(void) { return cu.pos; }
int cob_unstr_tally(void) { return cu.tally; }
int cob_unstr_overflow(void) { return cu.overflow || cu.pos <= cu.slen; }

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

/* ====================================================================== */
/* INDEXED files.  The data file named by ASSIGN holds recsize-byte slots  */
/* in arrival order; beside it "<name>.key" holds one B+tree per key in a  */
/* 4K-paged file (libcob/btree.h): the RECORD KEY's tree and one for each  */
/* ALTERNATE RECORD KEY, entries of (key bytes, arrival number, slot) in    */
/* key-then-arrival order, a bitmap of the live slots so a DELETEd slot is */
/* reused, and a free chain of pages.  docs/indexed.md describes it.       */
/*                                                                         */
/* Until 2026-09-04 the key file was a sorted array per key ("S32KEY01",   */
/* prime key only; "S32KEY02", alternates too) loaded whole at OPEN.  Such */
/* a file is still read, once: OPEN converts it to the tree in place.      */
/* ====================================================================== */

/* The legacy key table, kept only to read an S32KEY01/02 file once and
 * convert it to the tree (below). */
typedef struct {
    unsigned char *e;       /* count entries of (klen bytes, u32 slot, u32 seq), sorted by key then seq */
    unsigned count, cap;
    unsigned off, klen;     /* the key's place in the record */
    int dups;               /* WITH DUPLICATES */
} cob_ktab;

/* The indexed file's keys live in `<data>.key`, one B+tree per key in a
 * 4K-paged file (libcob/btree.h; docs/indexed.md).  Program-visible
 * behaviour is what the sorted arrays gave: a random READ finds the key,
 * READ NEXT walks the key of reference, START positions, duplicates
 * come back in arrival order, and the status codes follow the 1985 text.
 * The cursor is the (key, arrival) of the next entry to deliver, so a
 * WRITE, REWRITE or DELETE between two READ NEXTs needs no fixing up. */
typedef struct {
    btf bt;                 /* the key file: bt.k[0] the prime key, bt.k[i] the i-th alternate */
    int ref;                /* key of reference: 0 the prime key, i the i-th alternate */
    int have_cur;           /* the cursor is set (else READ NEXT starts at the front) */
    unsigned char cur[BT_KEYMAX + 4];   /* next (key, arrival) to deliver, inclusive */
    int last_slot;          /* slot the last READ delivered, for REWRITE/DELETE; -1 */
    unsigned char *tmp;     /* a record's worth of scratch */
} cob_idx;

#define KEYMAGIC1 "S32KEY01"
#define KEYMAGIC2 "S32KEY02"

static unsigned tab_esize(const cob_ktab *t) { return t->klen + 8; }
static unsigned char *tab_entry(const cob_ktab *t, unsigned i) { return t->e + (size_t)i * tab_esize(t); }
static void put_u32(unsigned char *p, unsigned v) { p[0] = (unsigned char)v; p[1] = (unsigned char)(v >> 8); p[2] = (unsigned char)(v >> 16); p[3] = (unsigned char)(v >> 24); }
static unsigned get_u32(const unsigned char *p) { return p[0] | (p[1] << 8) | (p[2] << 16) | ((unsigned)p[3] << 24); }
static unsigned tab_slot(const cob_ktab *t, unsigned i) { return get_u32(tab_entry(t, i) + t->klen); }
static unsigned tab_seq(const cob_ktab *t, unsigned i) { return get_u32(tab_entry(t, i) + t->klen + 4); }

static const char *key_file_name(cob_file *f)
{
    static char name[300];
    const char *d = file_name(f);
    size_t n = strlen(d);
    if (n > 290) n = 290;
    memcpy(name, d, n); memcpy(name + n, ".key", 5);
    return name;
}

static int slot_read_to(cob_file *f, unsigned slot, unsigned char *buf)
{
    FILE *fp = (FILE *)f->fp;
    if (fseek(fp, (long)slot * (long)f->recsize, 0) != 0) return 0;
    return fread(buf, 1, f->recsize, fp) == f->recsize;
}
static int slot_read(cob_file *f, unsigned slot) { return slot_read_to(f, slot, (unsigned char *)f->record); }
static int slot_write(cob_file *f, unsigned slot)
{
    FILE *fp = (FILE *)f->fp;
    if (fseek(fp, (long)slot * (long)f->recsize, 0) != 0) return 0;
    if (fwrite(f->record, 1, f->recsize, fp) != f->recsize) return 0;
    fflush(fp);
    return 1;
}

/* The page cache: S32_INDEX_CACHE pages, else a sixteenth of the heap the
 * program was linked with, 16..256 pages -- there is no sbrk. */
static unsigned idx_cache_pages(void)
{
    const char *e = getenv("S32_INDEX_CACHE");
    if (e && *e) { unsigned v = (unsigned)atoi(e); return v < BT_MINCACHE ? BT_MINCACHE : v; }
    size_t heap = (size_t)(__heap_end - __heap_start);
    unsigned n = (unsigned)(heap / 16 / BT_PAGE);
    return n < BT_MINCACHE ? BT_MINCACHE : n > 256 ? 256 : n;
}

/* the keys as the FD declares them: bt.k[0] prime, then the alternates */
static unsigned idx_fd_keys(cob_file *f, btkey *keys)
{
    unsigned n = f->naltkeys < BT_MAXKEYS - 1 ? f->naltkeys : BT_MAXKEYS - 1;
    /* the prime entry carries each alternate's arrival number, so a DELETE or
     * a REWRITE that moves an alternate key removes its entry exactly */
    keys[0].off = f->keyoff; keys[0].klen = f->keylen; keys[0].dups = 0; keys[0].root = 0; keys[0].count = 0; keys[0].extra = 4 * n;
    for (unsigned i = 0; i < n; i++) {
        const cob_altkey *a = &f->altkeys[i];
        keys[i + 1].off = a->offset; keys[i + 1].klen = a->len; keys[i + 1].dups = (unsigned)a->dups; keys[i + 1].root = 0; keys[i + 1].count = 0; keys[i + 1].extra = 0;
    }
    return n + 1;
}

static int idx_keys_match(cob_file *f, const btf *b)
{
    btkey keys[BT_MAXKEYS];
    unsigned n = idx_fd_keys(f, keys);
    if (b->recsize != f->recsize || b->keyoff != f->keyoff || b->keylen != f->keylen) return 0;
    if (b->nkeys != n) return 0;
    for (unsigned i = 0; i < n; i++)
        if (b->k[i].off != keys[i].off || b->k[i].klen != keys[i].klen || b->k[i].dups != keys[i].dups || b->k[i].extra != keys[i].extra) return 0;
    return 1;
}

/* --- reading an S32KEY01/02 file, once, to convert it --- */

static unsigned tab_find(const cob_ktab *t, const unsigned char *k, unsigned len, int *found)
{
    unsigned lo = 0, hi = t->count;
    while (lo < hi) {
        unsigned mid = (lo + hi) / 2;
        int c = memcmp(tab_entry(t, mid), k, len);
        if (c < 0) lo = mid + 1; else hi = mid;
    }
    *found = (lo < t->count && !memcmp(tab_entry(t, lo), k, len));
    return lo;
}

static int old_load(cob_file *f, FILE *kf, const unsigned char *h, cob_ktab *prime, cob_ktab **alt_out, unsigned *nalt_out, unsigned *nslots, unsigned *seq)
{
    if (get_u32(h + 8) != f->recsize || get_u32(h + 12) != f->keyoff || get_u32(h + 16) != f->keylen) return 0;
    int v2 = !memcmp(h, KEYMAGIC2, 8);
    memset(prime, 0, sizeof *prime);
    prime->off = f->keyoff; prime->klen = f->keylen;
    prime->count = get_u32(h + 20); *nslots = get_u32(h + 24);
    prime->cap = prime->count ? prime->count : 1;
    prime->e = malloc((size_t)prime->cap * tab_esize(prime));
    if (!prime->e) cob_fatal("out of memory for the key table");
    unsigned nalt = f->naltkeys;
    cob_ktab *alt = nalt ? calloc(nalt, sizeof *alt) : 0;
    if (nalt && !alt) cob_fatal("out of memory for the key tables");
    for (unsigned i = 0; i < nalt; i++) { alt[i].off = f->altkeys[i].offset; alt[i].klen = f->altkeys[i].len; alt[i].dups = (int)f->altkeys[i].dups; }
    int alts_ok = 0;
    if (v2) {
        *seq = get_u32(h + 28);
        size_t want = (size_t)prime->count * tab_esize(prime);
        if (fread(prime->e, 1, want, kf) != want) return 0;
        unsigned char ah[4];
        if (fread(ah, 1, 4, kf) == 4 && get_u32(ah) == nalt) {
            alts_ok = 1;
            for (unsigned a = 0; a < nalt && alts_ok; a++) {
                unsigned char th[16];
                cob_ktab *t = &alt[a];
                if (fread(th, 1, 16, kf) != 16 || get_u32(th) != t->off || get_u32(th + 4) != t->klen || get_u32(th + 8) != (unsigned)t->dups) { alts_ok = 0; break; }
                t->count = get_u32(th + 12); t->cap = t->count ? t->count : 1;
                t->e = malloc((size_t)t->cap * tab_esize(t));
                if (!t->e) cob_fatal("out of memory for a key table");
                size_t w = (size_t)t->count * tab_esize(t);
                if (fread(t->e, 1, w, kf) != w) { alts_ok = 0; break; }
            }
        }
    } else {
        for (unsigned i = 0; i < prime->count; i++) {
            unsigned char *e = tab_entry(prime, i);
            if (fread(e, 1, f->keylen + 4, kf) != f->keylen + 4) return 0;
            put_u32(e + f->keylen + 4, i);
        }
        *seq = prime->count;
    }
    *alt_out = alt; *nalt_out = alts_ok ? nalt : 0;    /* 0: the alternates are rebuilt from the records */
    return 1;
}

/* the alternate trees from the records the prime tree names */
static void idx_alt_rebuild(cob_file *f, cob_idx *x)
{
    btf *b = &x->bt;
    unsigned char zero[BT_KEYMAX + 4], ka[BT_KEYMAX + 4];
    memset(zero, 0, sizeof zero);
    unsigned page, ix;
    if (!bt_first_ge(b, 0, zero, b->k[0].klen + 4, &page, &ix)) return;
    unsigned char ex[4 * BT_MAXKEYS];
    do {
        unsigned slot = bt_read(b, 0, page, ix, ka), seq = bt_getbe(ka + b->k[0].klen);
        if (!slot_read_to(f, slot, x->tmp)) continue;
        for (unsigned a = 1; a < b->nkeys; a++) { bt_insert(b, a, x->tmp + b->k[a].off, seq, slot); bt_putbe(ex + 4 * (a - 1), seq); }
        if (b->nkeys > 1) bt_extra_set(b, 0, page, ix, ex);
    } while (bt_step(b, &page, &ix));
}

/* an old key file becomes a tree file in place */
static int idx_migrate(cob_file *f, cob_idx *x, FILE *kf, const unsigned char *h)
{
    cob_ktab prime, *alt = 0; unsigned nalt = 0, nslots = 0, seq = 0;
    int ok = old_load(f, kf, h, &prime, &alt, &nalt, &nslots, &seq);
    fclose(kf);
    if (!ok) { free(prime.e); if (alt) { for (unsigned a = 0; a < f->naltkeys; a++) free(alt[a].e); free(alt); } return 0; }
    btkey keys[BT_MAXKEYS];
    unsigned nkeys = idx_fd_keys(f, keys);
    if (!bt_create(&x->bt, key_file_name(f), idx_cache_pages(), f->recsize, f->keyoff, f->keylen, keys, nkeys, cob_fatal)) return 0;
    btf *b = &x->bt;
    b->nslots = nslots; b->seq = seq;
    unsigned maxslot = nslots;
    for (unsigned i = 0; i < prime.count; i++) if (tab_slot(&prime, i) >= maxslot) maxslot = tab_slot(&prime, i) + 1;
    unsigned char *ex = 0;
    if (nalt) {                                      /* slot -> each alternate's arrival, from the old tables */
        ex = calloc((size_t)maxslot, 4 * nalt);
        if (!ex) cob_fatal("out of memory converting the key file");
        for (unsigned a = 0; a < nalt; a++)
            for (unsigned i = 0; i < alt[a].count; i++) bt_putbe(ex + (size_t)tab_slot(&alt[a], i) * 4 * nalt + 4 * a, tab_seq(&alt[a], i));
    }
    for (unsigned i = 0; i < prime.count; i++) {
        unsigned slot = tab_slot(&prime, i);
        bt_insert_x(b, 0, tab_entry(&prime, i), tab_seq(&prime, i), slot, ex ? ex + (size_t)slot * 4 * nalt : 0);
        bt_slot_set(b, slot, 1);
        if (slot >= b->nslots) b->nslots = slot + 1;
    }
    if (nalt) for (unsigned a = 0; a < nalt; a++)
        for (unsigned i = 0; i < alt[a].count; i++) bt_insert(b, a + 1, tab_entry(&alt[a], i), tab_seq(&alt[a], i), tab_slot(&alt[a], i));
    else idx_alt_rebuild(f, x);
    free(ex);
    b->hdr_dirty = 1;
    free(prime.e); if (alt) { for (unsigned a = 0; a < f->naltkeys; a++) free(alt[a].e); free(alt); }
    return 1;
}

/* the FD's alternates differ from the key file's: rebuild the key file
 * from its prime tree and the records */
static int idx_rekey(cob_file *f, cob_idx *x)
{
    btf old = x->bt;
    char tmpname[320];
    snprintf(tmpname, sizeof tmpname, "%s.new", key_file_name(f));
    btkey keys[BT_MAXKEYS];
    unsigned nkeys = idx_fd_keys(f, keys);
    btf nb;
    if (!bt_create(&nb, tmpname, idx_cache_pages(), f->recsize, f->keyoff, f->keylen, keys, nkeys, cob_fatal)) return 0;
    nb.nslots = old.nslots; nb.seq = old.seq;
    unsigned char zero[BT_KEYMAX + 4], ka[BT_KEYMAX + 4];
    memset(zero, 0, sizeof zero);
    unsigned page, ix;
    if (bt_first_ge(&old, 0, zero, old.k[0].klen + 4, &page, &ix)) {
        do {
            unsigned slot = bt_read(&old, 0, page, ix, ka);
            bt_insert(&nb, 0, ka, bt_getbe(ka + old.k[0].klen), slot);
            bt_slot_set(&nb, slot, 1);
        } while (bt_step(&old, &page, &ix));
    }
    bt_close(&old, 0);
    x->bt = nb;
    idx_alt_rebuild(f, x);
    bt_close(&x->bt, 1);
    if (rename(tmpname, key_file_name(f)) != 0) return 0;
    return bt_open(&x->bt, key_file_name(f), 0, idx_cache_pages(), cob_fatal);
}

/* the key file for an existing data file: ours, or an old one converted; 0 = 39 */
static int idx_load(cob_file *f, cob_idx *x, int rdonly)
{
    const char *kn = key_file_name(f);
    if (bt_open(&x->bt, kn, rdonly, idx_cache_pages(), cob_fatal)) {
        if (idx_keys_match(f, &x->bt)) return 1;
        if (x->bt.recsize != f->recsize || x->bt.keyoff != f->keyoff || x->bt.keylen != f->keylen) { bt_close(&x->bt, 0); return 0; }
        return idx_rekey(f, x);                       /* the alternates changed under the file */
    }
    FILE *kf = fopen(kn, "rb");
    if (!kf) return 0;
    unsigned char h[32];
    if (fread(h, 1, 32, kf) != 32 || (memcmp(h, KEYMAGIC1, 8) && memcmp(h, KEYMAGIC2, 8))) { fclose(kf); return 0; }
    return idx_migrate(f, x, kf, h);
}

static cob_idx *idx_new(cob_file *f)
{
    cob_idx *x = calloc(1, sizeof *x);
    if (!x) cob_fatal("out of memory");
    x->bt.fd = -1;
    x->last_slot = -1; x->ref = 0; x->have_cur = 0;
    x->tmp = malloc(f->recsize ? f->recsize : 1);
    if (!x->tmp) cob_fatal("out of memory");
    return x;
}

static void idx_free(cob_idx *x)
{
    if (!x) return;
    if (x->bt.fd >= 0) bt_close(&x->bt, 0);
    free(x->tmp); free(x);
}

static int idx_open(cob_file *f, int mode)
{
    const char *name = file_name(f);
    if (f->keylen == 0 || f->keylen > BT_KEYMAX) cob_fatal("RECORD KEY must be 1 to 255 bytes");
    if (f->naltkeys > BT_MAXKEYS - 1) cob_fatal("more than 16 ALTERNATE RECORD KEYs");
    if (f->locked) return file_result(f, "38", "OPEN of a file closed WITH LOCK");
    cob_idx *x = idx_new(f);
    FILE *fp;
    if (mode == COB_OPEN_OUTPUT) {
        fp = fopen(name, "w+b");
        if (!fp) { idx_free(x); return file_result(f, "30", name); }
        btkey keys[BT_MAXKEYS];
        unsigned nkeys = idx_fd_keys(f, keys);
        if (!bt_create(&x->bt, key_file_name(f), idx_cache_pages(), f->recsize, f->keyoff, f->keylen, keys, nkeys, cob_fatal)) { fclose(fp); idx_free(x); return file_result(f, "30", "key file"); }
    } else {
        fp = fopen(name, mode == COB_OPEN_INPUT ? "rb" : "r+b");
        if (!fp) {
            if (!f->optional) { idx_free(x); return file_result(f, "35", name); }
            if (mode == COB_OPEN_INPUT) { idx_free(x); f->open_mode = (unsigned char)mode; f->fp = 0; f->at_eof = 1; return file_result(f, "05", name); }
            /* OPTIONAL, absent, I-O or EXTEND: the file comes into being, empty */
            fp = fopen(name, "w+b");
            if (!fp) { idx_free(x); return file_result(f, "30", name); }
            btkey keys[BT_MAXKEYS];
            unsigned nkeys = idx_fd_keys(f, keys);
            if (!bt_create(&x->bt, key_file_name(f), idx_cache_pages(), f->recsize, f->keyoff, f->keylen, keys, nkeys, cob_fatal)) { fclose(fp); idx_free(x); return file_result(f, "30", "key file"); }
            f->fp = fp; f->idx = x; f->open_mode = (unsigned char)mode; f->at_eof = 0; f->eof_seen = 0;
            return file_result(f, "05", name);
        }
        f->fp = fp;
        if (!idx_load(f, x, 0)) { fclose(fp); f->fp = 0; idx_free(x); return file_result(f, "39", "key file missing or does not match the FD"); }
    }
    f->fp = fp; f->idx = x; f->open_mode = (unsigned char)mode; f->at_eof = 0; f->eof_seen = 0;
    return file_result(f, "00", name);
}

static int idx_close(cob_file *f)
{
    cob_idx *x = f->idx;
    if (x) {
        if (x->bt.fd >= 0) bt_close(&x->bt, 1);
        idx_free(x);
    }
    if (f->fp) fclose((FILE *)f->fp);
    f->fp = 0; f->idx = 0; f->open_mode = 0; f->at_eof = 0;
    return file_result(f, "00", "key file");
}

/* the first entry of key ki whose key equals k (on the whole key); page/ix out */
static int idx_find(cob_idx *x, unsigned ki, const unsigned char *k, unsigned *page, unsigned *ix)
{
    btf *b = &x->bt;
    unsigned kl = b->k[ki].klen;
    unsigned char ka[BT_KEYMAX + 4];
    memcpy(ka, k, kl); memset(ka + kl, 0, 4);
    if (!bt_first_ge(b, ki, ka, kl + 4, page, ix)) return 0;
    bt_read(b, ki, *page, *ix, ka);
    return memcmp(ka, k, kl) == 0;
}

/* an alternate key that would duplicate an existing record's, at a slot
 * other than `skip`: 0 (a 22) if the key forbids duplicates, else the
 * 02 is remembered */
static int alt_check(cob_idx *x, const unsigned char *rec, unsigned skip, int *dup02)
{
    btf *b = &x->bt;
    for (unsigned a = 1; a < b->nkeys; a++) {
        unsigned page, ix, kl = b->k[a].klen;
        if (!idx_find(x, a, rec + b->k[a].off, &page, &ix)) continue;
        int other = 0;
        unsigned char ka[BT_KEYMAX + 4];
        do {
            unsigned slot = bt_read(b, a, page, ix, ka);
            if (memcmp(ka, rec + b->k[a].off, kl) != 0) break;
            if (slot != skip) other = 1;
        } while (!other && bt_step(b, &page, &ix));
        if (!other) continue;
        if (!b->k[a].dups) return 0;
        *dup02 = 1;
    }
    return 1;
}

/* the cursor: deliver from (key, arrival) on */
static void idx_cursor_at(cob_idx *x, unsigned ki, unsigned page, unsigned ix)
{
    bt_read(&x->bt, ki, page, ix, x->cur);
    x->have_cur = 1;
}
/* ... after the entry just delivered */
static void idx_cursor_after(cob_idx *x, unsigned ki, const unsigned char *ka)
{
    unsigned kl = x->bt.k[ki].klen;
    memcpy(x->cur, ka, kl + 4);
    bt_putbe(x->cur + kl, bt_getbe(ka + kl) + 1);
    x->have_cur = 1;
}

static int idx_write(cob_file *f)
{
    cob_idx *x = f->idx;
    if (!x) return file_result(f, "48", "WRITE to an OPTIONAL file that is absent");
    btf *b = &x->bt;
    const unsigned char *k = (const unsigned char *)f->record + f->keyoff;
    unsigned page, ix;
    if (f->access == 0 && b->k[0].count) {                       /* sequential access: keys must ascend */
        unsigned char last[BT_KEYMAX + 4];
        bt_last(b, 0, &page, &ix); bt_read(b, 0, page, ix, last);
        if (memcmp(last, k, f->keylen) >= 0) return file_result(f, "21", "");
    }
    if (idx_find(x, 0, k, &page, &ix)) return file_result(f, "22", "");    /* duplicate prime key */
    int dup02 = 0;
    if (!alt_check(x, (const unsigned char *)f->record, (unsigned)-1, &dup02)) return file_result(f, "22", "");
    unsigned slot = bt_slot_alloc(b);
    if (!slot_write(f, slot)) { bt_slot_set(b, slot, 0); return file_result(f, "30", "write failed"); }
    unsigned seq = b->seq++; b->hdr_dirty = 1;
    unsigned char ex[4 * BT_MAXKEYS];
    for (unsigned a = 1; a < b->nkeys; a++) bt_putbe(ex + 4 * (a - 1), seq);
    bt_insert_x(b, 0, k, seq, slot, ex);
    for (unsigned a = 1; a < b->nkeys; a++) bt_insert(b, a, (const unsigned char *)f->record + b->k[a].off, seq, slot);
    x->last_slot = -1;
    return file_result(f, dup02 ? "02" : "00", "");
}

/* READ with KEY (random): by the prime key (ki 0) or an alternate (ki i),
 * whose value is what the record's field holds; that key becomes the key
 * of reference.  02: another record has the same alternate key. */
int cob_read_key(cob_file *f, int ki)
{
    if (!f->open_mode) return file_result(f, "47", "READ of a file not open");
    if (f->org == COB_ORG_RELATIVE) return rel_read_key(f);
    if (f->org != COB_ORG_INDEXED) cob_fatal("READ ... KEY on a file that is not INDEXED");
    cob_idx *x = f->idx;
    if (!x) return file_result(f, "23", "");
    btf *b = &x->bt;
    if (ki < 0 || (unsigned)ki >= b->nkeys) cob_fatal("READ ... KEY: no such key");
    unsigned kl = b->k[ki].klen, page, ix;
    unsigned char key[BT_KEYMAX], ka[BT_KEYMAX + 4], nk[BT_KEYMAX + 4];
    memcpy(key, f->record + b->k[ki].off, kl);
    if (!idx_find(x, ki, key, &page, &ix)) { x->last_slot = -1; return file_result(f, "23", ""); }
    unsigned slot = bt_read(b, ki, page, ix, ka);
    if (!slot_read(f, slot)) return file_result(f, "30", "read failed");
    x->ref = ki; idx_cursor_after(x, ki, ka); x->last_slot = (int)slot; f->at_eof = 0; f->eof_seen = 0;
    int more = b->k[ki].dups && bt_step(b, &page, &ix) && (bt_read(b, ki, page, ix, nk), !memcmp(nk, key, kl));
    return file_result(f, more ? "02" : "00", "");
}

/* READ NEXT: along the key of reference; 02 when the next record has
 * the same (duplicate-allowing) key value as this one */
static int idx_read_next(cob_file *f)
{
    cob_idx *x = f->idx;
    if (!x) return file_result(f, "10", "");
    if (f->at_eof) { if (f->eof_seen) return file_result(f, "46", ""); f->eof_seen = 1; return file_result(f, "10", ""); }
    btf *b = &x->bt;
    unsigned ki = (unsigned)x->ref, kl = b->k[ki].klen, page, ix;
    if (!x->have_cur) { memset(x->cur, 0, kl + 4); x->have_cur = 1; }
    if (!bt_first_ge(b, ki, x->cur, kl + 4, &page, &ix)) { f->at_eof = 1; f->eof_seen = 1; x->last_slot = -1; return file_result(f, "10", ""); }
    unsigned char ka[BT_KEYMAX + 4], nk[BT_KEYMAX + 4];
    unsigned slot = bt_read(b, ki, page, ix, ka);
    if (!slot_read(f, slot)) return file_result(f, "30", "read failed");
    x->last_slot = (int)slot; idx_cursor_after(x, ki, ka);
    int more = b->k[ki].dups && bt_step(b, &page, &ix) && (bt_read(b, ki, page, ix, nk), !memcmp(nk, ka, kl));
    return file_result(f, more ? "02" : "00", "");
}

/* START: position on key ki, comparing its first len bytes (len < the
 * key's length: a data item that begins where the key begins) with the
 * record area's.  op: 0 =, 1 >, 2 >=, 3 <, 4 <=.  The key becomes the
 * key of reference. */
int cob_start(cob_file *f, int op, int ki, int len)
{
    if (!f->open_mode) return file_result(f, "47", "START of a file not open");
    if (f->org == COB_ORG_RELATIVE) return rel_start(f, op);
    if (f->org != COB_ORG_INDEXED) cob_fatal("START on a file that is not INDEXED");
    cob_idx *x = f->idx;
    if (!x) return file_result(f, "23", "");
    btf *b = &x->bt;
    if (ki < 0 || (unsigned)ki >= b->nkeys) cob_fatal("START ... KEY: no such key");
    unsigned kl = b->k[ki].klen;
    unsigned n = (len > 0 && (unsigned)len < kl) ? (unsigned)len : kl;
    const unsigned char *k = (const unsigned char *)f->record + b->k[ki].off;
    unsigned char target[BT_KEYMAX + 4], ka[BT_KEYMAX + 4];
    memset(target, 0, sizeof target); memcpy(target, k, n);
    unsigned page, ix;
    int got = bt_first_ge(b, ki, target, kl + 4, &page, &ix);       /* first entry whose leading n bytes >= k */
    int found = got && (bt_read(b, ki, page, ix, ka), !memcmp(ka, k, n));
    int pos = 0;
    switch (op) {
    case 0: pos = found; break;
    case 2: pos = got; break;
    case 1: case 4:                                                  /* past every entry equal on n bytes */
        if (found) { do { if (!bt_step(b, &page, &ix)) { got = 0; break; } bt_read(b, ki, page, ix, ka); } while (!memcmp(ka, k, n)); }
        if (op == 1) pos = got;
        else pos = got ? bt_back(b, &page, &ix) : bt_last(b, ki, &page, &ix);
        break;
    case 3: pos = got ? bt_back(b, &page, &ix) : bt_last(b, ki, &page, &ix); break;
    }
    if (!pos) return file_result(f, "23", "");
    x->ref = ki; idx_cursor_at(x, ki, page, ix); x->last_slot = -1; f->at_eof = 0; f->eof_seen = 0;
    return file_result(f, "00", "");
}

/* REWRITE (indexed): the record whose prime key the area holds -- under
 * sequential access it must be the last one read (21 otherwise); an
 * alternate key may change, keeping the duplicates rule (22), 02 when a
 * duplicate-allowing one now duplicates */
static int idx_rewrite(cob_file *f)
{
    cob_idx *x = f->idx;
    btf *b = &x->bt;
    const unsigned char *k = (const unsigned char *)f->record + f->keyoff;
    unsigned page, ix;
    int found = idx_find(x, 0, k, &page, &ix);
    unsigned slot = found ? bt_read(b, 0, page, ix, 0) : 0;
    if (f->access == 0) {
        if (x->last_slot < 0) return file_result(f, "43", "");
        if (!found || slot != (unsigned)x->last_slot) return file_result(f, "21", "");
    } else if (!found) return file_result(f, "23", "");
    if (!slot_read_to(f, slot, x->tmp)) return file_result(f, "30", "read failed");
    int dup02 = 0;
    if (!alt_check(x, (const unsigned char *)f->record, slot, &dup02)) return file_result(f, "22", "");
    unsigned seq = b->seq++; b->hdr_dirty = 1;
    unsigned char ex[4 * BT_MAXKEYS];
    int moved = 0;
    if (b->nkeys > 1) bt_extra_get(b, 0, page, ix, ex);
    for (unsigned a = 1; a < b->nkeys; a++) {
        unsigned off = b->k[a].off, kl = b->k[a].klen;
        if (memcmp(x->tmp + off, f->record + off, kl) == 0) continue;
        if (!bt_remove_exact(b, a, x->tmp + off, bt_getbe(ex + 4 * (a - 1)))) bt_remove(b, a, x->tmp + off, slot);
        bt_insert(b, a, (const unsigned char *)f->record + off, seq, slot);
        bt_putbe(ex + 4 * (a - 1), seq); moved = 1;
    }
    if (moved) bt_extra_set(b, 0, page, ix, ex);
    if (!slot_write(f, slot)) return file_result(f, "30", "write failed");
    return file_result(f, dup02 ? "02" : "00", "");
}

/* REWRITE: indexed by key; sequential in place after a READ, by the
 * position libcob kept */
int cob_rewrite(cob_file *f, int reclen)
{
    if (!f->open_mode) return file_result(f, "49", "REWRITE of a file not open");
    if (f->open_mode != COB_OPEN_IO) return file_result(f, "49", "REWRITE needs OPEN I-O");
    if (f->org == COB_ORG_RELATIVE) return rel_rewrite(f);
    if (f->org == COB_ORG_INDEXED) return idx_rewrite(f);
    if (f->org == COB_ORG_SEQ) {
        /* the record last read, at the position libcob kept (the libc's
         * buffered stream reads ahead, so its own position is not it);
         * the same length, or 44 */
        FILE *fp = (FILE *)f->fp;
        if (!f->last_len) return file_result(f, "43", "");                 /* no READ before it */
        unsigned len = f->last_len;
        if (f->varying) {
            unsigned want = reclen > 0 ? (unsigned)reclen : len;
            if (f->dep_item) want = (unsigned)cob_get_num(f->dep_item, (const cob_desc *)f->dep_desc);
            if (want != len) return file_result(f, "44", "");
            if (fseek(fp, (long)(f->fpos - 4 - len), 0) != 0) return file_result(f, "30", "seek failed");
            unsigned char rdw[4] = { (unsigned char)((len + 4) >> 8), (unsigned char)((len + 4) & 255), 0, 0 };
            if (fwrite(rdw, 1, 4, fp) != 4 || fwrite(f->record, 1, len, fp) != len) return file_result(f, "30", "write failed");
        } else {
            if (reclen > 0 && (unsigned)reclen != len) return file_result(f, "44", "");
            if (fseek(fp, (long)(f->fpos - len), 0) != 0) return file_result(f, "30", "seek failed");
            if (fwrite(f->record, 1, len, fp) != len) return file_result(f, "30", "write failed");
        }
        fseek(fp, (long)f->fpos, 0);                    /* back to after the record; the read buffer refills */
        f->last_len = 0;
        return file_result(f, "00", "");
    }
    return file_result(f, "49", "REWRITE on a LINE SEQUENTIAL file");
}

/* DELETE: the record whose prime key is in the record area (random) or
 * the one last read (sequential access); every key forgets it and the
 * slot is free for a later WRITE */
int cob_delete(cob_file *f)
{
    if (!f->open_mode) return file_result(f, "49", "DELETE of a file not open");
    if (f->open_mode != COB_OPEN_IO) return file_result(f, "49", "DELETE needs OPEN I-O");
    if (f->org == COB_ORG_RELATIVE) return rel_delete(f);
    if (f->org != COB_ORG_INDEXED) cob_fatal("DELETE on a file that is not INDEXED");
    cob_idx *x = f->idx;
    btf *b = &x->bt;
    unsigned slot, page, ix;
    if (f->access == 0) {
        if (x->last_slot < 0) return file_result(f, "43", "");
        slot = (unsigned)x->last_slot;
        if (!slot_read_to(f, slot, x->tmp)) return file_result(f, "30", "read failed");
        if (!idx_find(x, 0, x->tmp + f->keyoff, &page, &ix)) return file_result(f, "23", "");
    } else {
        if (!idx_find(x, 0, (const unsigned char *)f->record + f->keyoff, &page, &ix)) return file_result(f, "23", "");
        slot = bt_read(b, 0, page, ix, 0);
        if (!slot_read_to(f, slot, x->tmp)) return file_result(f, "30", "read failed");
    }
    unsigned char ex[4 * BT_MAXKEYS];
    if (b->nkeys > 1) bt_extra_get(b, 0, page, ix, ex);
    for (unsigned a = 1; a < b->nkeys; a++)
        if (!bt_remove_exact(b, a, x->tmp + b->k[a].off, bt_getbe(ex + 4 * (a - 1)))) bt_remove(b, a, x->tmp + b->k[a].off, slot);
    bt_remove(b, 0, x->tmp + f->keyoff, slot);
    bt_slot_set(b, slot, 0);
    x->last_slot = -1;
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
    if (f->org != COB_ORG_LINESEQ && f->recsize) {
        /* a record-oriented print file: each line is one record, space-filled */
        unsigned m = (unsigned)n < f->recsize ? (unsigned)n : f->recsize;
        fwrite(p, 1, m, (FILE *)f->fp);
        for (unsigned k = m; k < f->recsize; k++) fputc(' ', (FILE *)f->fp);
        r->line_counter++;
        return;
    }
    while (n > 0 && p[n - 1] == ' ') n--;
    if (n) fwrite(p, 1, n, (FILE *)f->fp);
    fputc('\n', (FILE *)f->fp);
    r->line_counter++;
}

static void rw_blank_to(cob_report *r, int line)   /* blank lines up to, not including, line */
{
    while (r->line_counter < line - 1) rw_put_line(r, "", 0);
}

/* INITIATE: LINE-COUNTER 0, PAGE-COUNTER 1 (X3.23 VIII-53 3.2.4); the
 * first page is begun by the first GENERATE without counting again */
void cob_rw_initiate(cob_report *r)
{
    r->line_counter = 0; r->page_counter = 1; r->body_seen = 0; r->page_started = 0;
    r->first_gen = 0; r->brk = 0; r->next_line = 0; r->next_page = 0; r->suppress = 0;
    r->gi_pending = ~0;
}
int cob_rw_page_started(cob_report *r) { return r->page_started; }
void cob_rw_first_page(cob_report *r) { r->page_started = 1; }

/* where the next line would land: a body line while no body group has
 * been presented on the page goes to FIRST DETAIL -- the 85 rule for
 * the first body group, and (measured on majesty's activity report) where
 * GnuCOBOL puts a group's remaining lines when they spill onto a new
 * page */
static int rw_target(cob_report *r, int abs, int plus, int is_body)
{
    if (abs) return abs;
    if (is_body && !r->body_seen) return r->next_line ? r->next_line : r->first_detail;
    return r->line_counter + plus;
}

/* 1 if the body group (first line abs/plus, `height` further lines of
 * relative extent) needs a new page before it is presented */
int cob_rw_fit(cob_report *r, int abs, int plus, int height)
{
    if (!r->page_started) return 1;
    if (r->next_page) return 1;
    int first = rw_target(r, abs, plus, 1);
    return first + height > r->last_detail;
}

static int rw_body_bound(cob_report *r, int is_body) { return is_body == 2 ? r->footing : r->last_detail; }

/* end the page: pad to PAGE LIMIT (when anything was printed), count it */
void cob_rw_page_end(cob_report *r)
{
    if (r->page_started) {
        while (r->line_counter < r->page_limit) rw_put_line(r, "", 0);
        r->page_counter++;
    }
    r->page_started = 1;
    r->line_counter = 0; r->body_seen = 0;
    r->next_page = 0;
    r->gi_pending = ~0;                 /* GROUP INDICATE prints again after a page advance */
}

/* a print line: its position is settled first -- blank lines up to it,
 * LINE-COUNTER set to it -- so a SOURCE of LINE-COUNTER on the line
 * prints the line's own number; then the fields; then the write */
void cob_rw_line_begin(cob_report *r, int abs, int plus, int is_body)
{
    int target = rw_target(r, abs, plus, is_body);
    if (target < r->line_counter + 1) target = r->line_counter + 1;
    rw_blank_to(r, target);
    r->line_counter = target;
    memset(rw_line, ' ', RW_WIDTH);
}

void cob_rw_field(int col, const cob_desc *dd, const void *src, const cob_desc *sd)
{
    if (col < 1 || col - 1 + (int)dd->size > RW_WIDTH) cob_fatal("report line wider than 512 columns");
    cob_move(src, sd, rw_line + col - 1, dd);
}

void cob_rw_line_write(cob_report *r, int is_body)
{
    r->line_counter--;                  /* rw_put_line counts it again */
    rw_put_line(r, rw_line, RW_WIDTH);
    if (is_body) { r->body_seen = 1; r->next_line = 0; }
}

/* a body line that would land past LAST DETAIL spills onto a new page:
 * the compiler renders the heading, and the line then lands on FIRST
 * DETAIL.  Measured on majesty's activity report (a group's trailing blank line
 * starting the next page) and its profit-and-loss report (the same, with
 * TERMINATE padding that page). */
int cob_rw_line_overflows(cob_report *r, int abs, int plus, int is_body)
{
    if (!is_body || !r->page_started) return 0;
    if (r->next_page) return 1;
    return rw_target(r, abs, plus, is_body) > rw_body_bound(r, is_body);
}

/* NEXT GROUP, after its group presented (X3.23 VIII: the final-setting
 * rules, reduced to their effect): an integer ahead of the position
 * moves LINE-COUNTER there; one behind saves itself for the next page;
 * PLUS spaces down; NEXT PAGE defers the next body group to a new page */
void cob_rw_next_group(cob_report *r, int kind, int n)
{
    if (kind == 1) {
        if (n > r->line_counter && n <= r->last_detail) { while (r->line_counter < n) rw_put_line(r, "", 0); }
        else { r->next_page = 1; r->next_line = n; }
    } else if (kind == 2) {
        if (r->line_counter + n > r->last_detail) r->next_page = 1;
        else for (int i = 0; i < n; i++) rw_put_line(r, "", 0);
    } else r->next_page = 1;
}

/* the REPORT FOOTING's place (X3.23 VIII Table 5, as cobc370 derived
 * it): after the PAGE FOOTING when one printed, else from the FOOTING
 * line; a footing that cannot fit takes a page of its own, with no
 * PAGE HEADING after the eject (2.21.4(3)a) */
void cob_rw_rf_begin(cob_report *r, int abs, int plus)
{
    if (!r->page_started) return;
    /* in this engine LINE-COUNTER is the paper: positioning writes lines */
    while (r->line_counter < r->footing) rw_put_line(r, "", 0);
    if (abs ? abs <= r->line_counter : r->line_counter + plus > r->page_limit) {
        while (r->line_counter < r->page_limit) rw_put_line(r, "", 0);
        r->page_counter++; r->line_counter = 0; r->body_seen = 0;
    }
}

void cob_rw_terminate(cob_report *r)
{
    if (r->page_started) while (r->line_counter < r->page_limit) rw_put_line(r, "", 0);
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

/* the slot's kind, and its item: the high kind bit says the slot holds
 * the address of a cell the compiler fills at ACCEPT/DISPLAY (a
 * subscripted, LINKAGE or EXTERNAL item) */
static int scr_kind(const cob_scr_field *f) { return f->kind & 0x7f; }
static void *scr_item(const cob_scr_field *f) { return (f->kind & 0x80) ? *(void **)f->item : f->item; }

static int scr_has_attr(const cob_scr_field *f)
{
    return (f->flags & (COB_SF_REVERSE | COB_SF_UNDERLINE | COB_SF_HIGHLIGHT | COB_SF_LOWLIGHT)) || f->fg != 255 || f->bg != 255;
}

/* COBOL's colour numbers (0 black, 1 blue, 2 green, 3 cyan, 4 red, 5
 * magenta, 6 yellow, 7 white) to ANSI's (1 red, 4 blue); 9 = default */
static int scr_ansi_colour(unsigned c) { static const int m[8] = { 0, 4, 2, 6, 1, 5, 3, 7 }; return c < 8 ? m[c] : 9; }

static void scr_attr(const cob_scr_field *f)
{
    if (!scr_has_attr(f)) return;
    if (f->flags & COB_SF_REVERSE) term_set_attr(7);
    else if (f->flags & COB_SF_UNDERLINE) term_set_attr(4);
    else if (f->flags & COB_SF_HIGHLIGHT) term_set_attr(1);
    else if (f->flags & COB_SF_LOWLIGHT) term_set_attr(2);
    else term_set_attr(0);
    if (f->fg != 255 || f->bg != 255) term_set_color(scr_ansi_colour(f->fg), scr_ansi_colour(f->bg));
}

static void scr_attr_off(const cob_scr_field *f)
{
    if (!scr_has_attr(f)) return;
    term_set_attr(0);
    if (f->fg != 255 || f->bg != 255) term_set_color(9, 9);
}

static void scr_puts_n(const char *p, unsigned n)
{
    for (unsigned i = 0; i < n; i++) term_putc(p[i]);
}

/* render a FROM/USING item through its picture into buf (width bytes) */
static void scr_render(const cob_scr_field *f, char *buf)
{
    if (scr_kind(f) == COB_SCR_VALUE) { memcpy(buf, f->value, f->width); return; }
    if (scr_kind(f) == COB_SCR_TO) { memset(buf, ' ', f->width); return; }
    cob_move(scr_item(f), (const cob_desc *)f->item_desc, buf, (const cob_desc *)f->pic);
}

static void scr_paint_text(const cob_scr_field *f, const char *buf)
{
    term_gotoxy(f->line, f->col);
    scr_attr(f);
    if (f->flags & COB_SF_SECURE) { for (unsigned i = 0; i < f->width; i++) term_putc(buf[i] == ' ' ? ' ' : '*'); }
    else scr_puts_n(buf, f->width);
    scr_attr_off(f);
}

static void scr_paint_field(const cob_scr_field *f)
{
    char buf[512];
    if (f->width > sizeof buf) cob_fatal("screen field wider than 512");
    scr_render(f, buf);
    scr_paint_text(f, buf);
}

void cob_screen_display(const cob_screen *s)
{
    term_need();
    term_begin_update();
    if (s->blank_screen) term_clear(0);
    for (unsigned i = 0; i < s->nfields; i++) scr_paint_field(&s->fields[i]);
    term_end_update();
}

/* ---- the focus loop (docs/screen.md, "the eventual target") ------------ */
/* Keys: the terminal's bytes, with the ANSI cursor sequences folded into
 * codes of their own; a lone Escape is K_ESC. */
enum { K_EOF = -1, K_ESC = 27, K_UP = 1001, K_DOWN, K_LEFT, K_RIGHT, K_HOME, K_END, K_DEL, K_BTAB, K_INS,
       K_PGUP, K_PGDN, K_F1, K_F12 = K_F1 + 11 };

static int scr_pending = -2;                    /* a byte read past a lone Escape */

static int scr_key(void)
{
    if (scr_pending != -2) { int k = scr_pending; scr_pending = -2; return k; }
    int k = term_getkey();
    if (k != 27) return k;
    if (!term_kbhit()) return K_ESC;
    int c = term_getkey();
    if (c == '[' || c == 'O') {
        int n = 0, d = term_getkey();
        while (d >= '0' && d <= '9') { n = n * 10 + (d - '0'); d = term_getkey(); }
        switch (d) {
        case 'A': return K_UP;
        case 'B': return K_DOWN;
        case 'C': return K_RIGHT;
        case 'D': return K_LEFT;
        case 'H': return K_HOME;
        case 'F': return K_END;
        case 'Z': return K_BTAB;
        case 'P': case 'Q': case 'R': case 'S':
            return d == 'P' ? K_F1 : d == 'Q' ? K_F1 + 1 : d == 'R' ? K_F1 + 2 : K_F1 + 3;   /* ESC O P..S: F1-F4 */
        case '~':
            if (n >= 11 && n <= 15) return K_F1 + (n - 11);          /* F1-F5 */
            if (n >= 17 && n <= 21) return K_F1 + 5 + (n - 17);      /* F6-F10 */
            if (n == 23 || n == 24) return K_F1 + 10 + (n - 23);     /* F11, F12 */
            return n == 3 ? K_DEL : n == 1 || n == 7 ? K_HOME : n == 4 || n == 8 ? K_END : n == 2 ? K_INS
                 : n == 5 ? K_PGUP : n == 6 ? K_PGDN : 0;
        default: return 0;
        }
    }
    if (c == 9) return K_BTAB;                  /* ESC TAB: back-tab on terminals without a Shift-Tab */
    scr_pending = c;                            /* a real Escape with the next keystroke behind it */
    return K_ESC;
}

/* SPECIAL-NAMES CRT STATUS IS item: the ACCEPT's ending, in GnuCOBOL's
 * numbering -- 0 ordinary, 1001+n function key n, 2001/2002 page up and
 * down, 2005 Escape.  A numeric item takes the number; a three-byte
 * alphanumeric the packed form GnuCOBOL writes; anything else the four
 * digits as text. */
static void *crt_item; static const cob_desc *crt_desc;

void cob_crt_status(void *item, const void *desc) { crt_item = item; crt_desc = desc; }

static void scr_set_status(int fret)
{
    if (!crt_item) return;
    const cob_desc *d = crt_desc;
    if (d->cat == COB_NUM) { cob_put_num(crt_item, d, fret, 0); return; }
    unsigned char *p = crt_item;
    if (d->size == 3) {
        p[0] = '0'; p[1] = 0; p[2] = 0;
        if (fret == 0) { p[1] = '0'; }
        else if (fret == 2005) p[0] = '1';
        else if (fret >= 1001 && fret <= 1064) { p[0] = '1'; p[1] = (unsigned char)(fret - 1000); }
        else if (fret >= 2001 && fret <= 2110) { p[0] = '2'; p[1] = (unsigned char)(fret - 2000); }
        else if (fret >= 8000) p[0] = '9';
        return;
    }
    char b[4]; int n = fret;
    for (int i = 3; i >= 0; i--) { b[i] = (char)('0' + n % 10); n /= 10; }
    for (unsigned i = 0; i < d->size; i++) p[i] = i < 4 ? (unsigned char)b[i] : ' ';
}

static int scr_key_status(int k)
{
    if (k >= K_F1 && k <= K_F12) return 1001 + (k - K_F1);
    return k == K_PGUP ? 2001 : k == K_PGDN ? 2002 : k == K_ESC ? 2005 : 0;
}

static int scr_is_numeric(const cob_scr_field *f)
{
    const cob_desc *d = (const cob_desc *)f->pic;
    return d && (d->cat == COB_NUM || d->cat == COB_NUM_ED);
}

/* the state of one input field under edit */
typedef struct {
    const cob_scr_field *f;
    char *buf;                 /* text: the characters; numeric: the rendering */
    unsigned pos;              /* text: the cursor */
    int numeric, neg, infrac, touched;
    int ni, nf, ni_max, nf_max, point;   /* numeric: digits typed each side of the point, the capacities, the point's column */
    char ibuf[20], fbuf[20];
} scr_edit;

/* the numeric value the digits typed so far stand for, at the picture's scale */
static long long scr_num_value(const scr_edit *e)
{
    long long v = 0;
    for (int i = 0; i < e->ni; i++) v = v * 10 + (e->ibuf[i] - '0');
    for (int i = 0; i < e->nf_max; i++) v = v * 10 + (i < e->nf ? e->fbuf[i] - '0' : 0);
    return e->neg ? -v : v;
}

static void scr_num_render(scr_edit *e)
{
    const cob_desc *d = (const cob_desc *)e->f->pic;
    cob_put_num_x(e->buf, d, scr_num_value(e), d->scale, 0);
}

/* the item's current value, as digits to edit in place */
static void scr_num_load(scr_edit *e)
{
    const cob_desc *d = (const cob_desc *)e->f->pic;
    long long v = scr_kind(e->f) == COB_SCR_USING ? cob_get_num(scr_item(e->f), (const cob_desc *)e->f->item_desc) : 0;
    int is = ((const cob_desc *)e->f->item_desc)->scale;
    if (scr_kind(e->f) == COB_SCR_USING && is != d->scale) v = is > d->scale ? div_pow10(v, is - d->scale, 0) : v * pow10tab[d->scale - is];
    e->neg = v < 0 && (d->flags & COB_F_SIGNED);
    unsigned long long mag = v < 0 ? 0 - (unsigned long long)v : (unsigned long long)v, fr;
    unsigned long long ip = udiv_pow10(mag, d->scale, &fr);
    char t[24]; mag_to_digits(ip, t, 20);
    int k = 0; while (k < 20 && t[k] == '0') k++;
    e->ni = 20 - k; if (e->ni > e->ni_max) { k += e->ni - e->ni_max; e->ni = e->ni_max; }
    memcpy(e->ibuf, t + k, (size_t)e->ni);
    e->nf = fr ? d->scale : 0;
    if (e->nf) mag_to_digits(fr, e->fbuf, e->nf);
    e->infrac = 0; e->touched = 0;
}

static unsigned scr_num_cursor(const scr_edit *e)
{
    unsigned w = e->f->width;
    if (e->infrac) { unsigned c = (unsigned)e->point + 1 + (unsigned)e->nf; return c < w ? c : w - 1; }
    if (e->nf_max == 0) return w - 1;
    return (unsigned)e->point;
}

static void scr_beep(void) { term_putc(7); }

/* entering a field: the cursor at its start; a numeric field takes the
 * next digit as a fresh entry (Enter alone keeps what it shows) */
static void scr_focus(scr_edit *e) { e->pos = 0; e->infrac = 0; e->touched = 0; }

/* leaving a field forwards: REQUIRED wants something in it, FULL wants it
 * empty or full */
static int scr_may_leave(const scr_edit *e)
{
    const cob_scr_field *f = e->f;
    if (f->flags & COB_SF_REQUIRED) {
        if (e->numeric ? scr_num_value(e) == 0 : strspn(e->buf, " ") >= f->width) return 0;
    }
    if ((f->flags & COB_SF_FULL) && !e->numeric) {
        unsigned n = f->width; while (n > 0 && e->buf[n - 1] == ' ') n--;
        if (n != 0 && n != f->width) return 0;
    }
    return 1;
}

void cob_screen_accept(const cob_screen *s)
{
    cob_screen_display(s);
    unsigned nin = 0;
    for (unsigned i = 0; i < s->nfields; i++)
        if (scr_kind(&s->fields[i]) == COB_SCR_TO || scr_kind(&s->fields[i]) == COB_SCR_USING) nin++;
    if (!nin) { scr_set_status(scr_key_status(scr_key())); return; }   /* nothing to type into: wait for a key */
    scr_edit *ed = calloc(nin, sizeof *ed);
    if (!ed) cob_fatal("out of memory");
    unsigned k = 0;
    for (unsigned i = 0; i < s->nfields; i++) {
        const cob_scr_field *f = &s->fields[i];
        if (scr_kind(f) != COB_SCR_TO && scr_kind(f) != COB_SCR_USING) continue;
        scr_edit *e = &ed[k++];
        e->f = f;
        e->buf = malloc(f->width + 1);
        if (!e->buf) cob_fatal("out of memory");
        scr_render(f, e->buf); e->buf[f->width] = 0;
        e->numeric = scr_is_numeric(f);
        if (e->numeric) {
            const cob_desc *d = (const cob_desc *)f->pic;
            e->nf_max = d->scale; e->ni_max = d->digits - d->scale;
            e->point = -1;
            if (d->pic) { const char *q = strchr(d->pic, '.'); if (q) e->point = (int)(q - d->pic); }
            if (e->point < 0) e->point = d->scale ? (int)f->width - d->scale - 1 : (int)f->width;   /* an assumed point: the fraction's first column, less one */
            scr_num_load(e);
        }
    }
    unsigned cur = 0;
    int done = 0, abandon = 0, fret = 0;
    while (!done) {
        scr_edit *e = &ed[cur];
        const cob_scr_field *f = e->f;
        term_gotoxy(f->line, f->col + (int)(e->numeric ? scr_num_cursor(e) : e->pos));
        int key = scr_key();
        if (key == K_EOF) { done = 1; break; }
        if (key == K_ESC) { done = 1; abandon = 1; fret = 2005; break; }
        if ((key >= K_F1 && key <= K_F12) || key == K_PGUP || key == K_PGDN) {
            fret = scr_key_status(key);         /* an exception key ends the ACCEPT, the fields kept */
            done = 1; break;
        }
        if (key == '\r' || key == '\n' || key == '\t' || key == K_DOWN) {
            if (!scr_may_leave(e)) { scr_beep(); continue; }
            if (cur + 1 < nin) { cur++; scr_focus(&ed[cur]); }
            else if (key == '\r' || key == '\n') done = 1;
            else { cur = 0; scr_focus(&ed[0]); }
            continue;
        }
        if (key == K_UP || key == K_BTAB) { cur = cur ? cur - 1 : nin - 1; scr_focus(&ed[cur]); continue; }
        if (e->numeric) {
            const cob_desc *d = (const cob_desc *)f->pic;
            int dp = cob_dp_comma ? ',' : '.';
            if ((key >= '0' && key <= '9') || key == dp || key == '-' || key == '+' || key == 8 || key == 127) {
                if (!e->touched) { e->ni = e->nf = 0; e->neg = 0; e->infrac = 0; e->touched = 1; }
            }
            if (key >= '0' && key <= '9') {
                if (e->infrac) { if (e->nf < e->nf_max) e->fbuf[e->nf++] = (char)key; else { scr_beep(); continue; } }
                else if (e->ni < e->ni_max) e->ibuf[e->ni++] = (char)key;
                else { scr_beep(); continue; }
                scr_num_render(e); scr_paint_text(f, e->buf);
                if ((f->flags & COB_SF_AUTO) && (e->infrac ? e->nf == e->nf_max : e->nf_max == 0 && e->ni == e->ni_max)) {
                    if (cur + 1 < nin) { cur++; scr_focus(&ed[cur]); } else done = 1;
                }
                continue;
            }
            if (key == dp) { if (e->nf_max) e->infrac = 1; else scr_beep(); continue; }
            if (key == '-' || key == '+') {
                if (!(d->flags & COB_F_SIGNED)) { scr_beep(); continue; }
                e->neg = key == '-' ? !e->neg : 0;
                scr_num_render(e); scr_paint_text(f, e->buf); continue;
            }
            if (key == 8 || key == 127) {
                if (e->infrac) { if (e->nf) e->nf--; else e->infrac = 0; }
                else if (e->ni) e->ni--;
                scr_num_render(e); scr_paint_text(f, e->buf); continue;
            }
            if (key == K_HOME) { e->ni = e->nf = 0; e->neg = 0; e->infrac = 0; e->touched = 1; scr_num_render(e); scr_paint_text(f, e->buf); continue; }
            continue;                           /* other keys: nothing */
        }
        /* a text field: edited where it sits */
        if (key == K_LEFT) { if (e->pos) e->pos--; continue; }
        if (key == K_RIGHT) { if (e->pos + 1 < f->width) e->pos++; continue; }
        if (key == K_HOME) { e->pos = 0; continue; }
        if (key == K_END) { unsigned n = f->width; while (n > 0 && e->buf[n - 1] == ' ') n--; e->pos = n < f->width ? n : f->width - 1; continue; }
        if (key == 8 || key == 127) {
            if (e->pos > 0) {
                e->pos--;
                memmove(e->buf + e->pos, e->buf + e->pos + 1, f->width - e->pos - 1);
                e->buf[f->width - 1] = ' ';
                scr_paint_text(f, e->buf);
            }
            continue;
        }
        if (key == K_DEL) {
            memmove(e->buf + e->pos, e->buf + e->pos + 1, f->width - e->pos - 1);
            e->buf[f->width - 1] = ' ';
            scr_paint_text(f, e->buf);
            continue;
        }
        if (key >= 32 && key < 127) {
            e->buf[e->pos] = (char)key;                 /* the cursor stands on pos already */
            scr_attr(f); term_putc((f->flags & COB_SF_SECURE) ? '*' : key); scr_attr_off(f);
            if (e->pos + 1 < f->width) e->pos++;
            else if (f->flags & COB_SF_AUTO) {
                if (!scr_may_leave(e)) { scr_beep(); continue; }
                if (cur + 1 < nin) { cur++; scr_focus(&ed[cur]); } else done = 1;
            }
            continue;
        }
        /* other control keys are ignored */
    }
    scr_set_status(fret);
    if (!abandon) {
        /* commit every input field into its item */
        for (unsigned i = 0; i < nin; i++) {
            scr_edit *e = &ed[i];
            const cob_scr_field *f = e->f;
            if (e->numeric) {
                const cob_desc *d = (const cob_desc *)f->pic;
                cob_put_num(scr_item(f), (const cob_desc *)f->item_desc, scr_num_value(e), d->scale);
            } else {
                cob_desc td; memset(&td, 0, sizeof td);
                td.cat = COB_ALNUM; td.usage = COB_U_DISPLAY; td.size = f->width;
                cob_move(e->buf, &td, scr_item(f), (const cob_desc *)f->item_desc);
            }
        }
    }
    for (unsigned i = 0; i < nin; i++) free(ed[i].buf);
    free(ed);
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
    if (a->scale > 0) v = div_pow10(v, a->scale, 0);
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

/* INSPECT, as X3.23 VIII (NC) describes it: one pass over the item, the
 * phrases tried in order at each position; a phrase that matches takes
 * the positions (tallied or replaced), which no later phrase sees; with
 * no match the position is passed over.  BEFORE/AFTER INITIAL bound
 * each phrase's range, found in the item's original contents from its
 * first character (AFTER absent: the phrase sees nothing; BEFORE
 * absent: to the end).  LEADING ends at the first position of its range
 * the phrase does not take; FIRST takes once.  The compiler registers
 * the phrases (cob_inspect_range before each, when it has one), runs
 * the pass, then adds each TALLYING phrase's count to its item. */
static const char *ci_before, *ci_after; static int ci_blen, ci_alen;
void cob_inspect_range(const char *bp, int bl, const char *ap, int al)
{
    ci_before = bp; ci_blen = bl; ci_after = ap; ci_alen = al;
}
static int ci_find(const char *p, int n, const char *x, int xl)
{
    if (xl < 1) return -1;
    for (int i = 0; i + xl <= n; i++) if (!memcmp(p + i, x, xl)) return i;
    return -1;
}
static struct {
    char *item; int n, np;
    struct { int tallying, kind, plen, lo, hi, done, count; const char *pat, *rep; } ph[32];
    char *real; int signpos, neg;       /* a signed DISPLAY item: inspected without its embedded sign */
} cin;
static char ci_copy[4096];
/* a signed numeric DISPLAY item with the sign in a digit is inspected as
 * though it had been moved to an unsigned item of the same size (X3.23
 * INSPECT general rules); the sign goes back afterwards */
void cob_inspect_begin(char *item, int n, const cob_desc *d)
{
    cin.item = item; cin.n = n; cin.np = 0; cin.real = NULL; cin.signpos = -1; cin.neg = 0;
    if (d && d->cat == COB_NUM && d->usage == COB_U_DISPLAY && (d->flags & COB_F_SIGNED) && !(d->flags & (COB_F_SEPLEAD | COB_F_SEPTRAIL)) && n > 0 && n <= (int)sizeof ci_copy) {
        int sp = (d->flags & COB_F_LEAD) ? 0 : n - 1;
        unsigned char c = (unsigned char)item[sp];
        memcpy(ci_copy, item, (size_t)n);
        if (c >= 'p' && c <= 'y') { cin.neg = 1; ci_copy[sp] = (char)(c - 'p' + '0'); }
        cin.real = item; cin.item = ci_copy; cin.signpos = sp;
    }
}
void cob_inspect_phrase(int tallying, int kind, const char *pat, int plen, const char *rep)
{
    if (cin.np == 32) cob_fatal("INSPECT: more than 32 phrases");
    int lo = 0, hi = cin.n;
    if (ci_after) { int i = ci_find(cin.item, cin.n, ci_after, ci_alen); lo = i < 0 ? cin.n : i + ci_alen; }
    if (ci_before) { int i = ci_find(cin.item, cin.n, ci_before, ci_blen); if (i >= 0) hi = i; }
    if (hi < lo) hi = lo;
    ci_before = ci_after = NULL; ci_blen = ci_alen = 0;
    cin.ph[cin.np].tallying = tallying; cin.ph[cin.np].kind = kind; cin.ph[cin.np].pat = pat;
    cin.ph[cin.np].plen = kind == 0 ? 1 : plen; cin.ph[cin.np].rep = rep;
    cin.ph[cin.np].lo = lo; cin.ph[cin.np].hi = hi; cin.ph[cin.np].done = 0; cin.ph[cin.np].count = 0;
    cin.np++;
}
void cob_inspect_run(void)
{
    for (int pos = 0; pos < cin.n; ) {
        int took = 0, taker = -1;
        for (int k = 0; k < cin.np && !took; k++) {
            if (cin.ph[k].done || pos < cin.ph[k].lo || pos + cin.ph[k].plen > cin.ph[k].hi) continue;
            int m = cin.ph[k].kind == 0 || !memcmp(cin.item + pos, cin.ph[k].pat, cin.ph[k].plen);
            if (!m) continue;
            if (cin.ph[k].tallying) cin.ph[k].count++;
            else memcpy(cin.item + pos, cin.ph[k].rep, cin.ph[k].plen);
            if (cin.ph[k].kind == 3) cin.ph[k].done = 1;
            took = cin.ph[k].plen; taker = k;
        }
        /* a LEADING phrase whose range has begun and which did not take
         * this position is over */
        for (int k = 0; k < cin.np; k++)
            if (cin.ph[k].kind == 2 && !cin.ph[k].done && pos >= cin.ph[k].lo && taker != k) cin.ph[k].done = 1;
        pos += took ? took : 1;
    }
    if (cin.real) {
        memcpy(cin.real, cin.item, (size_t)cin.n);
        unsigned char c = (unsigned char)cin.real[cin.signpos];
        if (cin.neg && c >= '0' && c <= '9') cin.real[cin.signpos] = (char)('p' + (c - '0'));
        cin.item = cin.real; cin.real = NULL;
    }
}
int cob_inspect_count(int k) { return cin.ph[k].count; }

/* CONVERTING from TO to [range]: one single-character replacing phrase per
 * character of `from`, all in the range set for the next phrase */
void cob_inspect_convert(const char *from, int n, const char *to)
{
    const char *bp = ci_before, *ap = ci_after; int bl = ci_blen, al = ci_alen;
    for (int i = 0; i < n; i++) {
        ci_before = bp; ci_after = ap; ci_blen = bl; ci_alen = al;
        cob_inspect_phrase(0, 1, from + i, 1, to + i);
    }
}

/* FUNCTION CURRENT-DATE: YYYYMMDDhhmmsshh followed by the offset from
 * UTC as +hhmm / -hhmm (21 characters); the guest clock through the
 * emulator, local time as the guest libc gives it */
/* The calendar functions of the 1989 addendum.  Integer 1 is 1601-01-01
 * (Gregorian); a date is yyyymmdd, a day yyyyddd.  An argument that is
 * not a valid date or day gives 0; a day count is ten DISPLAY digits, a
 * date eight, a day-of-year seven, as GnuCOBOL renders them. */
static long civil_to_days(long y, long m, long d)      /* days since 1601-01-01, +1 */
{
    y -= m <= 2;
    long era = (y >= 0 ? y : y - 399) / 400;
    long yoe = y - era * 400;
    long doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
    long doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    long from_0000_03_01 = era * 146097 + doe;
    return from_0000_03_01 - 584694 + 1;                /* 584694 = 1601-01-01 counted from 0000-03-01 */
}
static void days_to_civil(long n, long *y, long *m, long *d)
{
    long z = n - 1 + 584694;
    long era = (z >= 0 ? z : z - 146096) / 146097;
    long doe = z - era * 146097;
    long yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    long yy = yoe + era * 400;
    long doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    long mp = (5 * doy + 2) / 153;
    *d = doy - (153 * mp + 2) / 5 + 1;
    *m = mp + (mp < 10 ? 3 : -9);
    *y = yy + (*m <= 2);
}
static int leap(long y) { return (y % 4 == 0 && y % 100 != 0) || y % 400 == 0; }
static int valid_date(long y, long m, long d)
{
    static const int mdays[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    if (y < 1601 || y > 9999 || m < 1 || m > 12 || d < 1) return 0;
    return d <= mdays[m - 1] + (m == 2 && leap(y));
}
static char *fn_digits(long v, int n)
{
    char *b = fn_buffer(n);
    if (v < 0) v = 0;
    for (int i = n - 1; i >= 0; i--) { b[i] = (char)('0' + v % 10); v /= 10; }
    return b;
}
#define MAX_DAY 3067671L                                 /* 9999-12-31 */
/* ---- the 1989 amendment's numeric intrinsics -------------------------- */
/* Arguments arrive on the numeric stack; the result is a sign and 18
 * digits (SIGN LEADING SEPARATE), scale 0 for the integer class and 9
 * for the rest.  Exact i64 arithmetic where the function allows it,
 * libm doubles where it does not (the DBT runs those natively). */

static double fn_dbl(const cob_num *a)
{
    double d = (double)a->v; int sc = a->scale;
    while (sc > 18) { d /= 1e18; sc -= 18; }
    return d / (double)pow10tab[sc];
}

static char *fn_signed18(long long v)
{
    char *b = fn_buffer(20);
    unsigned long long m = v < 0 ? 0 - (unsigned long long)v : (unsigned long long)v;
    b[0] = v < 0 ? '-' : '+';
    mag_to_digits(m, b + 1, 18);
    b[19] = 0;
    return b;
}

static char *fn_from_dbl(double d)
{
    if (d > 999999999.0) d = 999999999.0;
    if (d < -999999999.0) d = -999999999.0;
    double r = d * 1e9;
    long long v = (long long)(r < 0 ? r - 0.5 : r + 0.5);
    return fn_signed18(v);
}

/* PCG-XSH-RR-64/32, the pcg32 of O'Neill's family -- the generator
 * ~/tinymux's svdrand.cpp uses when no 128-bit integers exist, which
 * is SLOW-32's situation exactly */
static unsigned long long pcg_state = 0x853c49e6748fea9bULL;
static const unsigned long long pcg_inc = 0xda3e39cb94b95bdbULL;   /* odd */

static unsigned pcg32(void)
{
    unsigned long long old = pcg_state;
    pcg_state = old * 6364136223846793005ULL + pcg_inc;
    unsigned xorshifted = (unsigned)(((old >> 18) ^ old) >> 27);
    unsigned rot = (unsigned)(old >> 59);
    return (xorshifted >> rot) | (xorshifted << ((32 - rot) & 31));
}

static int fn_cmp(const void *a, const void *b)
{
    double x = *(const double *)a, y = *(const double *)b;
    return x < y ? -1 : x > y ? 1 : 0;
}

char *cob_fn_num(int which, int n)
{
    cob_num *a = &nstk[nsp - n];               /* the n arguments, oldest first */
    char *res = 0;
    switch (which) {
    case COB_FN_MAX: case COB_FN_MIN: case COB_FN_ORD_MAX: case COB_FN_ORD_MIN:
    case COB_FN_SUM: case COB_FN_RANGE: case COB_FN_MIDRANGE: {
        /* exact: everything aligned to the widest scale */
        int sc = 0;
        for (int i = 0; i < n; i++) if (a[i].scale > sc) sc = a[i].scale;
        long long hi = 0, lo = 0, sum = 0; int ihi = 0, ilo = 0;
        for (int i = 0; i < n; i++) {
            long long v = cob_rescale(a[i].v, a[i].scale, sc);
            if (i == 0 || v > hi) { hi = v; ihi = i; }
            if (i == 0 || v < lo) { lo = v; ilo = i; }
            sum += v;
        }
        long long r9;
        switch (which) {
        case COB_FN_MAX: r9 = cob_rescale(hi, sc, 9); break;
        case COB_FN_MIN: r9 = cob_rescale(lo, sc, 9); break;
        case COB_FN_SUM: r9 = cob_rescale(sum, sc, 9); break;
        case COB_FN_RANGE: r9 = cob_rescale(hi - lo, sc, 9); break;
        case COB_FN_MIDRANGE: r9 = cob_rescale(hi + lo, sc, 9) / 2; break;
        case COB_FN_ORD_MAX: r9 = ihi + 1; break;
        default: r9 = ilo + 1; break;
        }
        res = fn_signed18(r9);
        break;
    }
    case COB_FN_MOD: {                          /* integer args; the result has the divisor's sign */
        long long x = a[0].v / pow10tab[a[0].scale], y = a[1].v / pow10tab[a[1].scale];
        if (y == 0) cob_fatal("FUNCTION MOD with a zero divisor");
        long long r = x % y;
        if (r != 0 && ((r < 0) != (y < 0))) r += y;
        res = fn_signed18(r);
        break;
    }
    case COB_FN_REM: {                          /* a - b * INTEGER-PART(a / b), exact at the common scale */
        int sc = a[0].scale > a[1].scale ? a[0].scale : a[1].scale;
        long long x = cob_rescale(a[0].v, a[0].scale, sc), y = cob_rescale(a[1].v, a[1].scale, sc);
        if (y == 0) cob_fatal("FUNCTION REM with a zero divisor");
        long long q = x / y;                    /* truncation is INTEGER-PART */
        res = fn_signed18(cob_rescale(x - q * y, sc, 9));
        break;
    }
    case COB_FN_INTEGER: {                      /* the greatest integer not greater */
        long long k = pow10tab[a[0].scale];
        long long q = a[0].v / k;
        if (a[0].v < 0 && q * k != a[0].v) q--;
        res = fn_signed18(q);
        break;
    }
    case COB_FN_INTEGER_PART:
        res = fn_signed18(a[0].v / pow10tab[a[0].scale]);
        break;
    case COB_FN_FACTORIAL: {
        long long k = a[0].v / pow10tab[a[0].scale], r = 1;
        if (k < 0 || k > 19) cob_fatal("FUNCTION FACTORIAL of a value outside 0-19");
        for (long long i = 2; i <= k; i++) r *= i;
        res = fn_signed18(r);
        break;
    }
    case COB_FN_MEAN: case COB_FN_MEDIAN: case COB_FN_VARIANCE: case COB_FN_STDDEV: {
        double *d = malloc((size_t)n * sizeof *d);
        if (!d) cob_fatal("out of memory");
        double sum = 0;
        for (int i = 0; i < n; i++) { d[i] = fn_dbl(&a[i]); sum += d[i]; }
        double m = sum / n, out;
        if (which == COB_FN_MEAN) out = m;
        else if (which == COB_FN_MEDIAN) {
            qsort(d, (size_t)n, sizeof *d, fn_cmp);
            out = (n & 1) ? d[n / 2] : (d[n / 2 - 1] + d[n / 2]) / 2;
        } else {
            double v = 0;
            for (int i = 0; i < n; i++) v += (d[i] - m) * (d[i] - m);
            v /= n;
            out = which == COB_FN_VARIANCE ? v : sqrt(v);
        }
        free(d);
        res = fn_from_dbl(out);
        break;
    }
    case COB_FN_SQRT: res = fn_from_dbl(sqrt(fn_dbl(&a[0]))); break;
    case COB_FN_LOG: res = fn_from_dbl(log(fn_dbl(&a[0]))); break;
    case COB_FN_LOG10: res = fn_from_dbl(log10(fn_dbl(&a[0]))); break;
    case COB_FN_SIN: res = fn_from_dbl(sin(fn_dbl(&a[0]))); break;
    case COB_FN_COS: res = fn_from_dbl(cos(fn_dbl(&a[0]))); break;
    case COB_FN_TAN: res = fn_from_dbl(tan(fn_dbl(&a[0]))); break;
    case COB_FN_ASIN: res = fn_from_dbl(asin(fn_dbl(&a[0]))); break;
    case COB_FN_ACOS: res = fn_from_dbl(acos(fn_dbl(&a[0]))); break;
    case COB_FN_ATAN: res = fn_from_dbl(atan(fn_dbl(&a[0]))); break;
    case COB_FN_ANNUITY: {
        double r = fn_dbl(&a[0]); long long p = a[1].v / pow10tab[a[1].scale];
        res = fn_from_dbl(r == 0 ? 1.0 / (double)p : r / (1.0 - pow(1.0 + r, (double)-p)));
        break;
    }
    case COB_FN_PRESENT_VALUE: {
        double r = fn_dbl(&a[0]), pv = 0, f = 1.0 + r;
        double acc = 1.0;
        for (int i = 1; i < n; i++) { acc *= f; pv += fn_dbl(&a[i]) / acc; }
        res = fn_from_dbl(pv);
        break;
    }
    case COB_FN_RANDOM: {
        if (n > 0) { pcg_state = (unsigned long long)(a[0].v / pow10tab[a[0].scale]) * 2u + 1u; pcg32(); pcg32(); }
        res = fn_from_dbl((double)pcg32() / 4294967296.0);
        break;
    }
    default: cob_fatal("unknown intrinsic function");
    }
    nsp -= n;
    return res;
}

/* MAX and MIN over alphanumeric arguments: the winning argument's
 * value, space-padded comparison in the native sequence; ORD-MAX and
 * ORD-MIN its position */
static const char *al_arg_p[64]; static int al_arg_n[64]; static int al_cnt;

void cob_fn_al_arg(const char *p, int n)
{
    if (al_cnt < 64) { al_arg_p[al_cnt] = p; al_arg_n[al_cnt] = n; al_cnt++; }
}

static int al_cmp(const char *a, int an, const char *b, int bn)
{
    int n = an > bn ? an : bn;
    for (int i = 0; i < n; i++) {
        unsigned char x = i < an ? (unsigned char)a[i] : ' ';
        unsigned char y = i < bn ? (unsigned char)b[i] : ' ';
        if (x != y) return x < y ? -1 : 1;
    }
    return 0;
}

char *cob_fn_al(int which, int fsize)
{
    int best = 0;
    for (int i = 1; i < al_cnt; i++) {
        int c = al_cmp(al_arg_p[i], al_arg_n[i], al_arg_p[best], al_arg_n[best]);
        if ((which == COB_FN_MAX || which == COB_FN_ORD_MAX) ? c > 0 : c < 0) best = i;
    }
    int n = al_cnt; al_cnt = 0;
    if (n == 0) cob_fatal("FUNCTION MAX/MIN with no arguments");
    if (which == COB_FN_ORD_MAX || which == COB_FN_ORD_MIN) return fn_signed18(best + 1);
    char *b = fn_buffer((unsigned)fsize + 1);
    memset(b, ' ', (size_t)fsize);
    memcpy(b, al_arg_p[best], (size_t)(al_arg_n[best] < fsize ? al_arg_n[best] : fsize));
    b[fsize] = 0;
    return b;
}

/* CHAR: the character at ordinal position n of the native sequence;
 * ORD: an argument character's position -- each the other's inverse */
char *cob_fn_char(int n)
{
    char *b = fn_buffer(2);
    b[0] = (char)(n - 1); b[1] = 0;
    return b;
}

char *cob_fn_ord(const char *p)
{
    return fn_signed18((unsigned char)p[0] + 1);
}

char *cob_fn_reverse(const char *p, int n)
{
    char *b = fn_buffer((unsigned)n + 1);
    for (int i = 0; i < n; i++) b[i] = p[n - 1 - i];
    b[n] = 0;
    return b;
}

/* NUMVAL / NUMVAL-C: the numeric value of a character string --
 * spaces, an optional sign either side, digits with one point (comma
 * under DECIMAL-POINT IS COMMA); NUMVAL-C also passes over the
 * currency sign and the grouping separators */
char *cob_fn_numval(const char *p, int n, int cform)
{
    int i = 0, neg = 0, seen_pt = 0, scale = 0;
    long long v = 0;
    int dp = cob_dp_comma ? ',' : '.', grp = cob_dp_comma ? '.' : ',';
    int any_digit = 0;
    while (i < n && p[i] == ' ') i++;
    if (i < n && (p[i] == '+' || p[i] == '-')) { neg = p[i] == '-'; i++; }
    for (; i < n; i++) {
        unsigned char c = (unsigned char)p[i];
        if (c >= '0' && c <= '9') { any_digit = 1; if (scale < 18 && v < pow10tab[17]) { v = v * 10 + (c - '0'); if (seen_pt) scale++; } }
        else if (c == dp && !seen_pt) seen_pt = 1;
        else if (cform && (c == grp || c == (unsigned char)cob_currency)) continue;
        else if (c == ' ' && !any_digit) continue;      /* spaces stand between the sign, the currency and the number */
        else break;
    }
    while (i < n && p[i] == ' ') i++;               /* the trailing sign may stand off from the number */
    if (i < n) {
        unsigned char c = (unsigned char)p[i];
        if (c == '+' || c == '-') neg = c == '-';
        else if (cform && (c == 'C' || c == 'c' || c == 'D' || c == 'd')) neg = 1;   /* CR / DB */
    }
    if (scale > 9) { v /= pow10tab[scale - 9]; scale = 9; }
    v *= pow10tab[9 - scale];
    return fn_signed18(neg ? -v : v);
}

char *cob_fn_integer_of_date(long ymd)
{
    long y = ymd / 10000, m = ymd / 100 % 100, d = ymd % 100;
    return fn_digits(valid_date(y, m, d) ? civil_to_days(y, m, d) : 0, 10);
}
char *cob_fn_date_of_integer(long n)
{
    if (n < 1 || n > MAX_DAY) return fn_digits(0, 8);
    long y, m, d; days_to_civil(n, &y, &m, &d);
    return fn_digits(y * 10000 + m * 100 + d, 8);
}
char *cob_fn_day_of_integer(long n)
{
    if (n < 1 || n > MAX_DAY) return fn_digits(0, 7);
    long y, m, d; days_to_civil(n, &y, &m, &d);
    return fn_digits(y * 1000 + (n - civil_to_days(y, 1, 1) + 1), 7);
}
char *cob_fn_integer_of_day(long yddd)
{
    long y = yddd / 1000, doy = yddd % 1000;
    if (y < 1601 || y > 9999 || doy < 1 || doy > 365 + leap(y)) return fn_digits(0, 10);
    return fn_digits(civil_to_days(y, 1, 1) + doy - 1, 10);
}

/* ACCEPT ... FROM DATE (YYMMDD) | DAY (YYDDD) | TIME (HHMMSShh) |
 * DAY-OF-WEEK (1 Monday .. 7 Sunday): the text's unsigned integer,
 * moved to the item by the MOVE rules (X3.23 6.2.4) */
void cob_accept_datetime(int which, void *dst, const cob_desc *dd)
{
    struct timespec ts; ts.tv_sec = 0; ts.tv_nsec = 0;
    int hund = 0;
    if (clock_gettime(0, &ts) == 0) hund = (int)(ts.tv_nsec / 10000000); else ts.tv_sec = time(0);
    time_t now = (time_t)ts.tv_sec;
    struct tm *t = localtime(&now);
    char b[16]; int n;
    switch (which) {
    case 0: n = snprintf(b, sizeof b, "%02d%02d%02d", t->tm_year % 100, t->tm_mon + 1, t->tm_mday); break;
    case 1: n = snprintf(b, sizeof b, "%02d%03d", t->tm_year % 100, t->tm_yday + 1); break;
    case 2: n = snprintf(b, sizeof b, "%02d%02d%02d%02d", t->tm_hour, t->tm_min, t->tm_sec, hund); break;
    default: n = snprintf(b, sizeof b, "%d", t->tm_wday == 0 ? 7 : t->tm_wday); break;
    }
    cob_desc sd; memset(&sd, 0, sizeof sd);
    sd.cat = COB_NUM; sd.usage = COB_U_DISPLAY; sd.digits = (unsigned char)n; sd.size = (unsigned)n;
    cob_move(b, &sd, dst, dd);
}

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

/* ACCEPT identifier: one line from standard input, without its newline,
 * moved as alphanumeric text.  At end of file the item is left as it was. */
void cob_accept_console(void *p, const cob_desc *d)
{
    char line[4096];
    if (!fgets(line, sizeof line, stdin)) return;
    int n = (int)strlen(line);
    while (n > 0 && (line[n - 1] == '\n' || line[n - 1] == '\r')) n--;
    /* a line longer than a numeric DISPLAY item arrives as characters,
     * left-justified and truncated (what the NIST suite and GnuCOBOL do);
     * otherwise it is moved as text, which converts */
    if (d->cat == COB_NUM && d->usage == COB_U_DISPLAY && n > (int)d->size) { memcpy(p, line, d->size); return; }
    put_text(p, d, line, n);
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
