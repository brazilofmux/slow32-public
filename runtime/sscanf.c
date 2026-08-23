/* sscanf - the subset a 1993 config parser expects, plus room to grow.
 * Conversions: %d %i %u %x %X %o %c %s %f %% with optional field width
 * and the l/h length modifiers (h accepted, stored as int; l on
 * integers stores long == int here; %lf stores double). Doom's
 * m_config was the first caller. */

#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <ctype.h>

static int scan_int(const char **sp, int base, int width, long long *out) {
    const char *s = *sp;
    const char *start;
    long long v = 0;
    int neg = 0, digits = 0;

    if (width <= 0) {
        width = 0x7FFFFFFF;
    }
    start = s;
    if (width > 0 && (*s == '+' || *s == '-')) {
        neg = (*s == '-');
        s++;
        width--;
    }
    if (base == 0) {
        base = 10;
        if (width >= 2 && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
            base = 16;
            s += 2;
            width -= 2;
        } else if (width >= 1 && s[0] == '0') {
            base = 8;
        }
    } else if (base == 16 && width >= 2 && s[0] == '0' &&
               (s[1] == 'x' || s[1] == 'X')) {
        s += 2;
        width -= 2;
    }
    while (width > 0) {
        int c = (unsigned char)*s;
        int d;
        if (c >= '0' && c <= '9') {
            d = c - '0';
        } else if (c >= 'a' && c <= 'f') {
            d = c - 'a' + 10;
        } else if (c >= 'A' && c <= 'F') {
            d = c - 'A' + 10;
        } else {
            break;
        }
        if (d >= base) {
            break;
        }
        v = v * base + d;
        s++;
        digits++;
        width--;
    }
    if (!digits) {
        *sp = start;
        return 0;
    }
    *out = neg ? -v : v;
    *sp = s;
    return 1;
}

int vsscanf(const char *str, const char *fmt, va_list ap) {
    const char *s = str;
    int converted = 0;

    while (*fmt) {
        if (isspace((unsigned char)*fmt)) {
            while (isspace((unsigned char)*s)) {
                s++;
            }
            fmt++;
            continue;
        }
        if (*fmt != '%') {
            if (*s != *fmt) {
                return converted;
            }
            s++;
            fmt++;
            continue;
        }
        fmt++;
        if (*fmt == '%') {
            if (*s != '%') {
                return converted;
            }
            s++;
            fmt++;
            continue;
        }
        {
            int suppress = 0, width = 0, longmod = 0;
            long long iv;
            if (*fmt == '*') {
                suppress = 1;
                fmt++;
            }
            while (*fmt >= '0' && *fmt <= '9') {
                width = width * 10 + (*fmt - '0');
                fmt++;
            }
            while (*fmt == 'l' || *fmt == 'h') {
                if (*fmt == 'l') {
                    longmod++;
                }
                fmt++;
            }
            switch (*fmt) {
            case 'c': {
                int n = width ? width : 1;
                char *dst = suppress ? NULL : va_arg(ap, char *);
                int i;
                for (i = 0; i < n; i++) {
                    if (!*s) {
                        return converted;
                    }
                    if (dst) {
                        dst[i] = *s;
                    }
                    s++;
                }
                if (!suppress) {
                    converted++;
                }
                break;
            }
            case 's': {
                char *dst = suppress ? NULL : va_arg(ap, char *);
                int n = 0;
                while (isspace((unsigned char)*s)) {
                    s++;
                }
                if (!*s) {
                    return converted;
                }
                while (*s && !isspace((unsigned char)*s) &&
                       (width == 0 || n < width)) {
                    if (dst) {
                        dst[n] = *s;
                    }
                    s++;
                    n++;
                }
                if (dst) {
                    dst[n] = '\0';
                    converted++;
                }
                break;
            }
            case 'd': case 'i': case 'u': case 'x': case 'X': case 'o': {
                int base = (*fmt == 'd' || *fmt == 'u') ? 10
                         : (*fmt == 'o') ? 8
                         : (*fmt == 'i') ? 0 : 16;
                while (isspace((unsigned char)*s)) {
                    s++;
                }
                if (!scan_int(&s, base, width, &iv)) {
                    return converted;
                }
                if (!suppress) {
                    if (longmod >= 2) {
                        *va_arg(ap, long long *) = iv;
                    } else {
                        *va_arg(ap, int *) = (int)iv;
                    }
                    converted++;
                }
                break;
            }
            case 'f': case 'g': case 'e': {
                char *end;
                double d;
                while (isspace((unsigned char)*s)) {
                    s++;
                }
                d = strtod(s, &end);
                if (end == s) {
                    return converted;
                }
                s = end;
                if (!suppress) {
                    if (longmod) {
                        *va_arg(ap, double *) = d;
                    } else {
                        *va_arg(ap, float *) = (float)d;
                    }
                    converted++;
                }
                break;
            }
            case 'n':
                if (!suppress) {
                    *va_arg(ap, int *) = (int)(s - str);
                }
                break;
            default:
                return converted;
            }
            fmt++;
        }
    }
    return converted;
}

int sscanf(const char *str, const char *fmt, ...) {
    va_list ap;
    int r;
    va_start(ap, fmt);
    r = vsscanf(str, fmt, ap);
    va_end(ap);
    return r;
}
