/* codepage.c -- the program's codepage, and text crossing into it
 * (codepage.h; docs/plans/xbase-utf8.md). */
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "codepage.h"
#include "../../common/s32utf.h"
#include "../../common/s32cp437.h"

static int g_cp = CP_437;

int cp_current(void) { return g_cp; }

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
