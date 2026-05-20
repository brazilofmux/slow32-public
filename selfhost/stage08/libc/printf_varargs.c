/* Selfhost libc: trimmed varargs formatter
 *
 * Supports: %d %i %u %x %X %o %p %c %s %% %lld %lli %llu %llx %llX %llo
 * Excludes: width/precision/flags, float.
 *
 * Compiled only by stage05+ compilers (not stage04 bootstrap compiler).
 */

struct _FILE {
    int fd;
    int flags;
    int error;
    int eof;
    int used;
};
typedef struct _FILE FILE;

int fputc(int c, FILE *fp);
int fputs(const char *s, FILE *fp);
FILE *stdout;

typedef char *va_list;

static int fmt_emit_char(int to_file, FILE *fp, char *out, unsigned int size, int *pos, int c) {
    if (to_file) {
        if (fputc(c, fp) < 0) return -1;
        *pos = *pos + 1;
        return 0;
    }
    if (size > 0 && (unsigned int)(*pos) + 1 < size) {
        out[*pos] = (char)c;
    }
    *pos = *pos + 1;
    return 0;
}

static int fmt_emit_str(int to_file, FILE *fp, char *out, unsigned int size, int *pos, const char *s) {
    const char *p;
    p = s;
    while (*p) {
        if (fmt_emit_char(to_file, fp, out, size, pos, *p) < 0) return -1;
        p = p + 1;
    }
    return 0;
}

static int fmt_u32(char *buf, unsigned int v, int base, int upper) {
    int i;
    unsigned int d;
    i = 0;
    if (v == 0) {
        buf[i] = '0';
        return 1;
    }
    while (v > 0) {
        d = v % (unsigned int)base;
        if (d < 10) buf[i] = '0' + (char)d;
        else buf[i] = (upper ? 'A' : 'a') + (char)(d - 10);
        v = v / (unsigned int)base;
        i = i + 1;
    }
    return i;
}

static int fmt_u64(char *buf, unsigned long long v, int base, int upper) {
    int i;
    int d;
    i = 0;
    if (v == 0) {
        buf[i] = '0';
        return 1;
    }
    while (v > 0) {
        d = (int)(v % (unsigned long long)base);
        if (d < 10) buf[i] = '0' + (char)d;
        else buf[i] = (upper ? 'A' : 'a') + (char)(d - 10);
        v = v / (unsigned long long)base;
        i = i + 1;
    }
    return i;
}

static int vf_core(FILE *fp, char *out, unsigned int size, const char *fmt, va_list ap, int to_file) {
    const char *p;
    int pos;
    int lmod;
    p = fmt;
    pos = 0;
    while (*p) {
        if (*p != '%') {
            if (fmt_emit_char(to_file, fp, out, size, &pos, *p) < 0) return -1;
            p = p + 1;
            continue;
        }
        p = p + 1;
        if (*p == 0) break;
        if (*p == '%') {
            if (fmt_emit_char(to_file, fp, out, size, &pos, '%') < 0) return -1;
            p = p + 1;
            continue;
        }
        /* Parse length modifier */
        lmod = 0;
        if (*p == 'l') {
            p = p + 1;
            lmod = 1;
            if (*p == 'l') {
                p = p + 1;
                lmod = 2;
            }
        }
        if (*p == 'c') {
            int c;
            c = va_arg(ap, int);
            if (fmt_emit_char(to_file, fp, out, size, &pos, c) < 0) return -1;
            p = p + 1;
            continue;
        }
        if (*p == 's') {
            const char *s;
            s = va_arg(ap, char *);
            if (!s) s = "(null)";
            if (fmt_emit_str(to_file, fp, out, size, &pos, s) < 0) return -1;
            p = p + 1;
            continue;
        }
        if (lmod == 2 && (*p == 'd' || *p == 'i')) {
            char tmp[24];
            long long v64;
            unsigned long long uv64;
            int n;
            int j;
            v64 = va_arg(ap, long long);
            if (v64 < 0) {
                if (fmt_emit_char(to_file, fp, out, size, &pos, '-') < 0) return -1;
                uv64 = (unsigned long long)(0 - v64);
            } else {
                uv64 = (unsigned long long)v64;
            }
            n = fmt_u64(tmp, uv64, 10, 0);
            j = n - 1;
            while (j >= 0) {
                if (fmt_emit_char(to_file, fp, out, size, &pos, tmp[j]) < 0) return -1;
                j = j - 1;
            }
            p = p + 1;
            continue;
        }
        if (*p == 'd' || *p == 'i') {
            char tmp[16];
            int v;
            unsigned int uv;
            int n;
            int j;
            v = va_arg(ap, int);
            if (v < 0) {
                if (fmt_emit_char(to_file, fp, out, size, &pos, '-') < 0) return -1;
                uv = (unsigned int)(0 - v);
            } else {
                uv = (unsigned int)v;
            }
            n = fmt_u32(tmp, uv, 10, 0);
            j = n - 1;
            while (j >= 0) {
                if (fmt_emit_char(to_file, fp, out, size, &pos, tmp[j]) < 0) return -1;
                j = j - 1;
            }
            p = p + 1;
            continue;
        }
        if (lmod == 2 && (*p == 'u' || *p == 'x' || *p == 'X' || *p == 'o')) {
            char tmp[24];
            int n;
            int j;
            int base;
            int upper;
            base = 10;
            upper = 0;
            if (*p == 'x' || *p == 'X') base = 16;
            if (*p == 'o') base = 8;
            if (*p == 'X') upper = 1;
            n = fmt_u64(tmp, va_arg(ap, unsigned long long), base, upper);
            j = n - 1;
            while (j >= 0) {
                if (fmt_emit_char(to_file, fp, out, size, &pos, tmp[j]) < 0) return -1;
                j = j - 1;
            }
            p = p + 1;
            continue;
        }
        if (*p == 'u' || *p == 'x' || *p == 'X' || *p == 'o') {
            char tmp2[16];
            int n2;
            int j2;
            int base;
            int upper;
            base = 10;
            upper = 0;
            if (*p == 'x' || *p == 'X') base = 16;
            if (*p == 'o') base = 8;
            if (*p == 'X') upper = 1;
            n2 = fmt_u32(tmp2, va_arg(ap, unsigned int), base, upper);
            j2 = n2 - 1;
            while (j2 >= 0) {
                if (fmt_emit_char(to_file, fp, out, size, &pos, tmp2[j2]) < 0) return -1;
                j2 = j2 - 1;
            }
            p = p + 1;
            continue;
        }
        if (*p == 'p') {
            char tmp3[16];
            unsigned int v2;
            int n3;
            int j3;
            v2 = (unsigned int)va_arg(ap, char *);
            if (fmt_emit_str(to_file, fp, out, size, &pos, "0x") < 0) return -1;
            n3 = fmt_u32(tmp3, v2, 16, 0);
            j3 = 8 - n3;
            while (j3 > 0) {
                if (fmt_emit_char(to_file, fp, out, size, &pos, '0') < 0) return -1;
                j3 = j3 - 1;
            }
            j3 = n3 - 1;
            while (j3 >= 0) {
                if (fmt_emit_char(to_file, fp, out, size, &pos, tmp3[j3]) < 0) return -1;
                j3 = j3 - 1;
            }
            p = p + 1;
            continue;
        }
        if (fmt_emit_char(to_file, fp, out, size, &pos, '%') < 0) return -1;
        if (fmt_emit_char(to_file, fp, out, size, &pos, *p) < 0) return -1;
        p = p + 1;
    }
    if (!to_file && size > 0) {
        if ((unsigned int)pos < size) out[pos] = 0;
        else out[size - 1] = 0;
    }
    return pos;
}

int vsnprintf(char *str, unsigned int size, const char *format, va_list ap) {
    return vf_core((FILE *)0, str, size, format, ap, 0);
}

int snprintf(char *str, unsigned int size, const char *format, ...) {
    va_list ap;
    int rc;
    va_start(ap, format);
    rc = vsnprintf(str, size, format, ap);
    va_end(ap);
    return rc;
}

int vsprintf(char *str, const char *format, va_list ap) {
    return vsnprintf(str, 2147483647, format, ap);
}

int sprintf(char *str, const char *format, ...) {
    va_list ap;
    int rc;
    va_start(ap, format);
    rc = vsprintf(str, format, ap);
    va_end(ap);
    return rc;
}

int vfprintf(FILE *stream, const char *format, va_list ap) {
    return vf_core(stream, (char *)0, 0, format, ap, 1);
}

int fprintf(FILE *stream, const char *format, ...) {
    va_list ap;
    int rc;
    va_start(ap, format);
    rc = vfprintf(stream, format, ap);
    va_end(ap);
    return rc;
}

int vprintf(const char *format, va_list ap) {
    return vfprintf(stdout, format, ap);
}

int printf(const char *format, ...) {
    va_list ap;
    int rc;
    va_start(ap, format);
    rc = vprintf(format, ap);
    va_end(ap);
    return rc;
}
