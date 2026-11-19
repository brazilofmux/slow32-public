/* codepage.c -- the program's codepage, and text crossing into it
 * (codepage.h; docs/plans/xbase-utf8.md). */
#include <locale.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "codepage.h"
#include "../../common/s32utf.h"
#include "../../common/s32cp437.h"

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

/* UTF-8 to CP437, in place: never longer */
static void utf8_to_437(char *s) {
    unsigned char *p = (unsigned char *)s, *o = p;
    size_t n = strlen(s);
    while (*p) {
        uint32_t cp;
        size_t k;
        int b;
        if (*p < 0x80) { *o++ = *p++; n--; continue; }
        k = s32u_decode(p, n, &cp);
        b = s32_cp437_from_cp(cp);
        *o++ = (unsigned char)(b < 0 ? '?' : b);
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
