#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#define UTIL_NO_PRINTF_WRAP
#include "util.h"

void str_upper(char *s) {
    while (*s) {
        if (*s >= 'a' && *s <= 'z')
            *s -= 32;
        s++;
    }
}

int str_icmp(const char *a, const char *b) {
    while (*a && *b) {
        int ca = *a, cb = *b;
        if (ca >= 'a' && ca <= 'z') ca -= 32;
        if (cb >= 'a' && cb <= 'z') cb -= 32;
        if (ca != cb) return ca - cb;
        a++;
        b++;
    }
    return (unsigned char)*a - (unsigned char)*b;
}

int str_imatch(const char *input, const char *prefix) {
    while (*prefix) {
        int ci = *input, cp = *prefix;
        if (ci >= 'a' && ci <= 'z') ci -= 32;
        if (cp >= 'a' && cp <= 'z') cp -= 32;
        if (ci != cp) return 0;
        input++;
        prefix++;
    }
    /* Word boundary: end of string, or whitespace/punctuation after prefix */
    if (*input == '\0' || *input == ' ' || *input == '\t' ||
        *input == ',' || *input == '(' || *input == '"' || *input == '\'')
        return 1;
    return 0;
}

char *skip_ws(const char *s) {
    while (*s == ' ' || *s == '\t') s++;
    return (char *)s;
}

void trim_right(char *s) {
    int len = strlen(s);
    while (len > 0 && (s[len-1] == ' ' || s[len-1] == '\t' ||
                       s[len-1] == '\r' || s[len-1] == '\n'))
        len--;
    s[len] = '\0';
}

int read_line(char *buf, int size) {
    int c, i = 0;
    fflush(stdout);
    while (i < size - 1) {
        c = getchar();
        if (c == -1) {  /* EOF */
            if (i == 0) return -1;
            break;
        }
        if (c == '\n') break;
        if (c == '\r') continue;
        buf[i++] = c;
    }
    buf[i] = '\0';
    return i;
}

void str_copy(char *dst, const char *src, int n) {
    /* length + memcpy: the byte loop showed in every profile (lines, names,
     * values are copied constantly), and memcpy is the optimised routine */
    size_t len = strlen(src);
    if (n <= 0) return;
    if (len > (size_t)(n - 1)) len = (size_t)(n - 1);
    memcpy(dst, src, len);
    dst[len] = '\0';
}

void path_normalize(char *s) {
    while (*s) {
        if (*s == '\\') *s = '/';
        s++;
    }
}

int str_nicmp(const char *a, const char *b, int n) {
    int i;
    for (i = 0; i < n && *a && *b; i++, a++, b++) {
        int ca = *a, cb = *b;
        if (ca >= 'a' && ca <= 'z') ca -= 32;
        if (cb >= 'a' && cb <= 'z') cb -= 32;
        if (ca != cb) return ca - cb;
    }
    if (i == n) return 0;
    return (unsigned char)*a - (unsigned char)*b;
}

int is_ident_start(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_';
}

int is_ident_char(char c) {
    return is_ident_start(c) || (c >= '0' && c <= '9');
}

/* Case-insensitive wildcard match: '*' matches any sequence, '?' matches any char */
int str_like(const char *text, const char *pattern) {
    const char *t = text;
    const char *p = pattern;
    const char *star = NULL;
    const char *star_text = NULL;

    for (;;) {
        char pc, tc;
        if (*p == '*') {
            while (*p == '*') p++;
            if (*p == '\0') return 1;
            star = p;
            star_text = t;
            continue;
        }

        if (*p == '\0') return *t == '\0';

        if (*t == '\0') {
            /* Remaining pattern must be all '*' to match */
            while (*p == '*') p++;
            return *p == '\0';
        }

        pc = *p;
        tc = *t;
        if (pc >= 'a' && pc <= 'z') pc -= 32;
        if (tc >= 'a' && tc <= 'z') tc -= 32;

        if (pc == '?' || pc == tc) {
            p++;
            t++;
            continue;
        }

        if (star) {
            star_text++;
            t = star_text;
            p = star;
            continue;
        }
        return 0;
    }
}

/* ---- Console output (see util.h) ---- */

static int g_at_bol = 1;

int  out_at_line_start(void)     { return g_at_bol; }
void out_set_line_start(int bol) { g_at_bol = bol; }

int out_printf(const char *fmt, ...) {
    char small[1024];
    char *buf = small;
    va_list ap;
    int len;

    va_start(ap, fmt);
    len = vsnprintf(small, sizeof(small), fmt, ap);
    va_end(ap);
    if (len < 0)
        return len;
    if (len >= (int)sizeof(small)) {
        buf = malloc((size_t)len + 1);
        if (!buf)
            return -1;
        va_start(ap, fmt);
        vsnprintf(buf, (size_t)len + 1, fmt, ap);
        va_end(ap);
    }
    if (len > 0) {
        if (!g_at_bol && buf[len - 1] == '\n' && buf[0] != '\n')
            fputc('\n', stdout);
        fputs(buf, stdout);
        g_at_bol = buf[len - 1] == '\n';
    }
    if (buf != small)
        free(buf);
    return len;
}
