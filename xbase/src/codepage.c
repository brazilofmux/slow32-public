/* codepage.c -- the program's codepage, and text crossing into it
 * (codepage.h; docs/plans/xbase-utf8.md). */
#include <locale.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "codepage.h"
#include "cptables.h"
#include "../../common/s32utf.h"
#include "../../common/s32cp437.h"
#include "utf/charset_approx.c"   /* libutf's approximations (sync-libutf.sh): one unit, no build change */

static int g_cp = CP_437;

static void utf8_to_437(char *s);
static void cp437_to_utf8(char *s, int size);

int cp_current(void) { return g_cp; }

void cp_set(int cp) {
    g_cp = cp == CP_UTF8 ? CP_UTF8 : CP_437;
    setlocale(LC_CTYPE, g_cp == CP_UTF8 ? "C.UTF-8" : "C");
}

const char *cp_name(int cp) { return cp == CP_UTF8 ? "UTF8" : "CP437"; }

int cp_by_name(const char *name) {
    char u[16];
    int i;
    for (i = 0; name[i] && i < 15; i++) {
        char c = name[i];
        u[i] = (char)(c >= 'a' && c <= 'z' ? c - 32 : c);
    }
    u[i] = '\0';
    if (!strcmp(u, "UTF8") || !strcmp(u, "UTF-8")) return CP_UTF8;
    if (!strcmp(u, "CP437") || !strcmp(u, "437")) return CP_437;
    return 0;
}

int cp_decode(const char *s, int n, unsigned *cp) {
    uint32_t c;
    size_t k;
    if (g_cp != CP_UTF8 || (unsigned char)s[0] < 0x80) { *cp = (unsigned char)s[0]; return 1; }
    k = s32u_decode((const unsigned char *)s, (size_t)n, &c);
    *cp = c;
    return (int)k;
}

int cp_encode(unsigned cp, char *out) {
    if (g_cp != CP_UTF8) { out[0] = (char)cp; return 1; }
    return s32u_encode(cp, (unsigned char *)out);
}

int cp_chars(const char *s, int n) {
    int i = 0, count = 0;
    if (g_cp != CP_UTF8) return n;
    while (i < n) {
        unsigned c;
        i += cp_decode(s + i, n - i, &c);
        count++;
    }
    return count;
}

int cp_offset(const char *s, int n, int k) {
    int i = 0;
    if (g_cp != CP_UTF8) return k < 0 ? 0 : k > n ? n : k;
    while (k > 0 && i < n) {
        unsigned c;
        i += cp_decode(s + i, n - i, &c);
        k--;
    }
    return i;
}

int cp_char_437(unsigned char b) {
    return g_cp == CP_UTF8 ? (int)s32_cp437_to_cp(b) : b;
}

int cp_width(unsigned cp) {
    if (g_cp != CP_UTF8) return 1;
    return s32u_cp_width(cp);
}

int cp_columns(const char *s, int n) {
    int i = 0, cols = 0;
    if (g_cp != CP_UTF8) return n;
    while (i < n) {
        unsigned c;
        i += cp_decode(s + i, n - i, &c);
        cols += s32u_cp_width(c);
    }
    return cols;
}

int cp_key(unsigned cp) { return cp > 0xFF ? (int)cp + CP_KEY_HIGH : (int)cp; }

int cp_key_char(int key) {
    if (key >= CP_KEY_HIGH) return key - CP_KEY_HIGH;
    if (key >= 32 && key <= 255 && key != 127) return key;
    return -1;
}

char *cp_transcode(const char *s, int from, int to) {
    const unsigned char *p;
    size_t n = strlen(s);
    char *out;
    for (p = (const unsigned char *)s; *p && *p < 0x80; p++) ;
    if (!*p || from == to) return NULL;
    out = malloc(n * 3 + 1);              /* a CP437 byte is at most three in UTF-8 */
    if (!out) return NULL;
    if (from == CP_UTF8) {                /* to CP437: never longer */
        memcpy(out, s, n + 1);
        utf8_to_437(out);
    } else {
        memcpy(out, s, n + 1);
        cp437_to_utf8(out, (int)(n * 3 + 1));
    }
    return out;
}

int cp_is_utf8(const char *s, size_t n) {
    const unsigned char *p = (const unsigned char *)s;
    int high = 0;
    size_t i = 0;
    while (i < n) {
        uint32_t cp;
        size_t k;
        if (p[i] < 0x80) { i++; continue; }
        high = 1;
        k = s32u_decode(p + i, n - i, &cp);
        /* U+FFFD is ill-formed input unless it was written as itself */
        if (cp == S32U_REPL && !(k == 3 && p[i] == 0xEF && p[i + 1] == 0xBF && p[i + 2] == 0xBD))
            return 0;
        i += k;
    }
    return high;
}

int cp_file_is_utf8(FILE *fp) {
    char buf[4096];
    char *all = NULL;
    size_t len = 0, n;
    int r;
    while ((n = fread(buf, 1, sizeof buf, fp)) > 0) {
        char *grown = realloc(all, len + n);
        if (!grown) { free(all); rewind(fp); return 0; }
        all = grown;
        memcpy(all + len, buf, n);
        len += n;
    }
    r = all ? cp_is_utf8(all, len) : 0;
    free(all);
    rewind(fp);
    return r;
}

/* ---- the single-byte codepages ---- */

int cp_from_mark(unsigned char mark) {
    return mark == 0x01 ? CP_437 : mark == 0x02 ? CP_850 : mark == 0x03 ? CP_1252 : 0;
}

static uint32_t byte_to_cp(int cp, unsigned char b) {
    if (b < 0x80) return b;
    if (cp == CP_850) return cp850_hi[b - 0x80];
    if (cp == CP_1252) return cp1252_hi[b - 0x80];
    return s32_cp437_to_cp(b);
}

static int cp_to_byte(int cp, uint32_t u) {
    int i;
    const unsigned short *t;
    if (u < 0x80) return (int)u;
    if (cp == CP_437) return s32_cp437_from_cp(u);
    t = cp == CP_850 ? cp850_hi : cp1252_hi;
    for (i = 0; i < 128; i++) if (t[i] == u) return 0x80 + i;
    return -1;
}

/* one character's UTF-8 through a libutf DFA: the byte it maps to */
static int dfa_byte(const unsigned char *itt, const unsigned short *sot, const unsigned short *sbt,
                    int accept, const unsigned char *p) {
    int state = 0;
    do {
        unsigned char col = itt[*p++];
        unsigned short off = sot[state];
        for (;;) {
            int y = sbt[off];
            if (y < 128) {                       /* a run */
                if (col < y) { state = sbt[off + 1]; break; }
                col = (unsigned char)(col - y);
                off += 2;
            } else {                             /* a copy */
                y = 256 - y;
                if (col < y) { state = sbt[off + col + 1]; break; }
                col = (unsigned char)(col - y);
                off = (unsigned short)(off + y + 1);
            }
        }
    } while (state < accept);
    return state - accept;
}

/* code point u in single-byte codepage cp: exactly, else the nearest
 * (ruling 2): CP437 by libutf's CP437 table, CP850 and 1252 by its
 * Latin-1 table then exactly; '?' at the last */
static int approx_byte(int cp, uint32_t u) {
    unsigned char e[4];
    int b = cp_to_byte(cp, u);
    if (b >= 0) return b;
    if (u > 0x10FFFF || (u >= 0xD800 && u <= 0xDFFF)) return '?';
    s32u_encode(u, e);
    if (cp == CP_437) return dfa_byte(tr_cp437_itt, tr_cp437_sot, tr_cp437_sbt, TR_CP437_ACCEPTING_STATES_START, e);
    b = dfa_byte(tr_latin1_itt, tr_latin1_sot, tr_latin1_sbt, TR_LATIN1_ACCEPTING_STATES_START, e);
    b = cp_to_byte(cp, (uint32_t)b);   /* Latin-1's byte is its code point */
    return b >= 0 ? b : '?';
}

int cp_convert(const char *s, int n, int from, int to, char *out, int cap) {
    const unsigned char *p = (const unsigned char *)s;
    int i = 0, o = 0;
    while (i < n) {
        uint32_t u;
        unsigned char e[4];
        int k, m;
        if (from == CP_UTF8) { k = (int)s32u_decode(p + i, (size_t)(n - i), &u); }
        else { u = byte_to_cp(from, p[i]); k = 1; }
        if (to == CP_UTF8) m = s32u_encode(u, e);
        else { e[0] = (unsigned char)approx_byte(to, u); m = 1; }
        if (o + m > cap - 1) break;
        memcpy(out + o, e, (size_t)m);
        o += m;
        i += k;
    }
    out[o] = '\0';
    return o;
}

/* UTF-8 to CP437, in place: never longer */
static void utf8_to_437(char *s) {
    unsigned char *p = (unsigned char *)s, *o = p;
    size_t n = strlen(s);
    while (*p) {
        uint32_t cp;
        size_t k;
        if (*p < 0x80) { *o++ = *p++; n--; continue; }
        k = s32u_decode(p, n, &cp);
        *o++ = (unsigned char)approx_byte(CP_437, cp);   /* the nearest (ruling 2) */
        p += k;
        n -= k;
    }
    *o = '\0';
}

/* CP437 to UTF-8 in a buffer of size bytes: what does not fit is cut at
 * a character */
static void cp437_to_utf8(char *s, int size) {
    char *out = malloc((size_t)size);
    unsigned char *p = (unsigned char *)s;
    int o = 0;
    if (!out) return;
    for (; *p; p++) {
        unsigned char b[4];
        int k = s32u_encode(s32_cp437_to_cp(*p), b);
        if (o + k >= size) break;
        memcpy(out + o, b, (size_t)k);
        o += k;
    }
    out[o] = '\0';
    memcpy(s, out, (size_t)o + 1);
    free(out);
}

void cp_source_line(char *line, int size, int from_utf8) {
    const unsigned char *p;
    for (p = (const unsigned char *)line; *p && *p < 0x80; p++) ;
    if (!*p) return;                              /* ASCII: the same in both */
    if (from_utf8 && g_cp == CP_437) utf8_to_437(line);
    else if (!from_utf8 && g_cp == CP_UTF8) cp437_to_utf8(line, size);
}

void cp_line_to_utf8(char *line, int size, int from_utf8) {
    const unsigned char *p;
    if (from_utf8) return;
    for (p = (const unsigned char *)line; *p && *p < 0x80; p++) ;
    if (*p) cp437_to_utf8(line, size);
}

/* cp_literal's conversions, by the literal's address and the codepage */
typedef struct lit { const char *src; int cp; char *text; struct lit *next; } lit_t;
static lit_t *lit_table[256];

const char *cp_literal(const char *utf8) {
    unsigned h = (unsigned)(((uintptr_t)utf8 >> 3) & 255);
    lit_t *e;
    if (g_cp == CP_UTF8) return utf8;
    for (e = lit_table[h]; e; e = e->next)
        if (e->src == utf8 && e->cp == g_cp) return e->text;
    e = malloc(sizeof *e);
    if (!e) return utf8;
    e->text = strdup(utf8);
    if (!e->text) { free(e); return utf8; }
    utf8_to_437(e->text);
    e->src = utf8;
    e->cp = g_cp;
    e->next = lit_table[h];
    lit_table[h] = e;
    return e->text;
}
