/* libcob -- the SLOW-32 COBOL runtime.
 *
 * Guest code, built by the SLOW-32 C toolchain (cobol/ is in the tree's
 * ordinary universe, so the host compiles it).  The compiler lowers each
 * verb either to a short inline sequence or to a call in here with a
 * descriptor it built; the runtime works in bytes and pictures and knows
 * nothing about the statement that called it (cobc370's COBSTR shape).
 *
 * Stage 1: program start/stop and DISPLAY.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---- output: DISPLAY goes to stdout, line-buffered by us ------------- */

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

/* ---- DISPLAY ---------------------------------------------------------- */

/* Alphanumeric item or literal, or a numeric DISPLAY item whose bytes are
 * already characters. */
void cob_display(const char *p, int n)
{
    out_bytes(p, n);
}

void cob_display_nl(void)
{
    out_char('\n');
    out_flush();
}

void cob_display_flush(void)
{
    out_flush();
}

/* Numeric DISPLAY item with a trailing overpunched sign (the implementor
 * convention for ASCII, matching GnuCOBOL: a negative value carries its
 * last digit as 'p'..'y').  DISPLAY shows a leading sign when the picture
 * is signed, the digits as stored, and a point where the scale says --
 * again GnuCOBOL's convention; the standard leaves it to the implementor. */
void cob_display_numdisp(const char *p, int n, int scale, int is_signed)
{
    char last = p[n - 1];
    int neg = is_signed && (last >= 'p' && last <= 'y');
    if (is_signed) out_char(neg ? '-' : '+');
    for (int i = 0; i < n; i++) {
        if (scale > 0 && i == n - scale) out_char('.');
        char c = p[i];
        if (i == n - 1 && neg) c = (char)(last - 'p' + '0');
        out_char(c);
    }
}

/* Emit a scaled integer as `digits` digit characters with a point where
 * the scale says, sign in front when the picture is signed. */
static void emit_scaled(unsigned long long mag, int neg, int digits, int scale,
                        int is_signed)
{
    char d[40];
    int  n = 0;
    if (is_signed) out_char(neg ? '-' : '+');
    /* digits, least significant first */
    for (int i = 0; i < digits; i++) { d[n++] = (char)('0' + mag % 10); mag /= 10; }
    for (int i = n - 1; i >= 0; i--) {
        if (scale > 0 && i == scale - 1) out_char('.');
        out_char(d[i]);
    }
}

/* USAGE COMP / COMP-5 / signed-int and friends: two's complement,
 * little-endian, `bytes` wide, holding the value scaled by 10^scale. */
void cob_display_bin(const unsigned char *p, int bytes, int digits, int scale,
                     int is_signed)
{
    unsigned long long u = 0;
    for (int i = bytes - 1; i >= 0; i--) u = (u << 8) | p[i];
    int neg = 0;
    if (is_signed && bytes < 8 && (u >> (bytes * 8 - 1)) & 1)
        u |= ~0ULL << (bytes * 8);          /* sign-extend */
    if (is_signed && (long long)u < 0) { neg = 1; u = (unsigned long long)(-(long long)u); }
    emit_scaled(u, neg, digits, scale, is_signed);
}

/* USAGE COMP-3: packed decimal, sign nibble last (C positive, D negative,
 * F unsigned). */
void cob_display_packed(const unsigned char *p, int digits, int scale,
                        int is_signed)
{
    int bytes = digits / 2 + 1;
    unsigned long long mag = 0;
    for (int i = 0; i < bytes; i++) {
        int hi = p[i] >> 4, lo = p[i] & 15;
        if (i < bytes - 1) { mag = mag * 10 + hi; mag = mag * 10 + lo; }
        else mag = mag * 10 + hi;
    }
    int sign = p[bytes - 1] & 15;
    emit_scaled(mag, sign == 0xD, digits, scale, is_signed);
}
