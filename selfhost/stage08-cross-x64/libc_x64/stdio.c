/* stdio.c — Buffered I/O, FILE streams, and formatted output (printf)
 *
 * Includes printf/fprintf to avoid cross-file global variable references
 * (stdin/stdout/stderr). The stage07 compiler doesn't support extern. */

#define NULL 0
#define EOF (-1)

#define O_RDONLY  0
#define O_WRONLY  1
#define O_RDWR   2
#define O_CREAT   64
#define O_TRUNC  512
#define O_APPEND 1024

#define SEEK_SET 0
#define SEEK_CUR 1

#define FILE_BUF_SIZE 4096
#define MAX_FILES     32

/* External functions */
int write(int fd, char *buf, int len);
int read(int fd, char *buf, int len);
int open(char *path, int flags, int mode);
int close(int fd);
int lseek(int fd, int offset, int whence);
char *malloc(int size);
void free(char *ptr);
int strlen(char *s);
char *memset(char *dst, int c, int n);

/* ============================================================================
 * FILE structure and globals
 * ============================================================================ */

struct _file {
    int fd;
    int flags;
    char *buf;
    int buf_pos;
    int buf_len;
    int buf_dirty;
};

#define FILE struct _file

static struct _file file_table[MAX_FILES];
static int file_init_done;

static struct _file _stdin_f;
static struct _file _stdout_f;
static struct _file _stderr_f;

FILE *stdin;
FILE *stdout;
FILE *stderr;

/* Public: callers may invoke this explicitly to ensure stdio is up
 * before any printf/fopen call.  s32-fast-x64.c does this in main(). */
void file_sys_init(void) {
    if (file_init_done) return;
    file_init_done = 1;

    _stdin_f.fd = 0;
    _stdin_f.flags = 1;
    _stdin_f.buf = malloc(FILE_BUF_SIZE);
    _stdin_f.buf_pos = 0;
    _stdin_f.buf_len = 0;
    stdin = &_stdin_f;

    _stdout_f.fd = 1;
    _stdout_f.flags = 2;
    _stdout_f.buf = malloc(FILE_BUF_SIZE);
    _stdout_f.buf_pos = 0;
    _stdout_f.buf_dirty = 0;
    stdout = &_stdout_f;

    _stderr_f.fd = 2;
    _stderr_f.flags = 2;
    _stderr_f.buf = 0;
    _stderr_f.buf_pos = 0;
    _stderr_f.buf_dirty = 0;
    stderr = &_stderr_f;
}

/* ============================================================================
 * Buffered I/O
 * ============================================================================ */

int fflush(FILE *f) {
    if (!f) return 0;
    if (f->buf && f->buf_dirty > 0) {
        write(f->fd, f->buf, f->buf_dirty);
        f->buf_dirty = 0;
    }
    return 0;
}

FILE *fopen(char *path, char *mode) {
    int flags;
    int fd;
    int i;
    FILE *f;

    file_sys_init();

    flags = O_RDONLY;
    if (mode[0] == 'w') {
        if (mode[1] == '+') flags = O_RDWR | O_CREAT | O_TRUNC;
        else flags = O_WRONLY | O_CREAT | O_TRUNC;
    } else if (mode[0] == 'a') {
        if (mode[1] == '+') flags = O_RDWR | O_CREAT | O_APPEND;
        else flags = O_WRONLY | O_CREAT | O_APPEND;
    } else if (mode[0] == 'r') {
        if (mode[1] == '+') flags = O_RDWR;
        else flags = O_RDONLY;
    }

    fd = open(path, flags, 420);
    if (fd < 0) return 0;

    f = 0;
    i = 0;
    while (i < MAX_FILES) {
        if (file_table[i].fd == 0 && file_table[i].flags == 0) {
            f = &file_table[i];
            break;
        }
        i = i + 1;
    }
    if (!f) {
        close(fd);
        return 0;
    }

    f->fd = fd;
    f->flags = 16;
    if (mode[0] == 'r') f->flags = f->flags | 1;
    if (mode[0] == 'w' || mode[0] == 'a') f->flags = f->flags | 2;
    if (mode[1] == '+') f->flags = f->flags | 1 | 2;
    f->buf = malloc(FILE_BUF_SIZE);
    f->buf_pos = 0;
    f->buf_len = 0;
    f->buf_dirty = 0;
    return f;
}

int fclose(FILE *f) {
    if (!f) return -1;
    fflush(f);
    if (f->flags & 16) close(f->fd);
    if (f->buf) free(f->buf);
    f->fd = 0;
    f->flags = 0;
    f->buf = 0;
    return 0;
}

int fputc(int c, FILE *f) {
    char ch;
    file_sys_init();
    ch = c;
    if (!f->buf) {
        write(f->fd, &ch, 1);
        return c;
    }
    f->buf[f->buf_dirty] = ch;
    f->buf_dirty = f->buf_dirty + 1;
    if (f->buf_dirty >= FILE_BUF_SIZE || ch == 10) {
        fflush(f);
    }
    return c;
}

int fputs(char *s, FILE *f) {
    while (*s) {
        fputc(*s, f);
        s = s + 1;
    }
    return 0;
}

int fgetc(FILE *f) {
    int n;
    file_sys_init();
    if (f->flags & 4) return EOF;
    if (f->buf_pos >= f->buf_len) {
        if (!f->buf) f->buf = malloc(FILE_BUF_SIZE);
        n = read(f->fd, f->buf, FILE_BUF_SIZE);
        if (n <= 0) {
            f->flags = f->flags | 4;
            return EOF;
        }
        f->buf_pos = 0;
        f->buf_len = n;
    }
    n = f->buf[f->buf_pos] & 255;
    f->buf_pos = f->buf_pos + 1;
    return n;
}

int fread(char *ptr, int size, int count, FILE *f) {
    int total;
    int got;
    int ch;
    if (size <= 0 || count <= 0) return 0;
    total = size * count;
    got = 0;
    while (got < total) {
        ch = fgetc(f);
        if (ch == EOF) break;
        ptr[got] = ch;
        got = got + 1;
    }
    return got / size;
}

int fwrite(char *ptr, int size, int count, FILE *f) {
    int total;
    int i;
    total = size * count;
    i = 0;
    while (i < total) {
        fputc(ptr[i], f);
        i = i + 1;
    }
    return count;
}

int fseek(FILE *f, int offset, int whence) {
    int rc;
    fflush(f);
    f->buf_pos = 0;
    f->buf_len = 0;
    f->flags = f->flags & ~4;
    rc = lseek(f->fd, offset, whence);
    if (rc < 0) return -1;
    return 0;
}

int ftell(FILE *f) {
    int pos;
    fflush(f);
    pos = lseek(f->fd, 0, SEEK_CUR);
    if (f->buf_len > f->buf_pos)
        pos = pos - (f->buf_len - f->buf_pos);
    return pos;
}

int putchar(int c) {
    file_sys_init();
    return fputc(c, stdout);
}

/* ============================================================================
 * Formatted output (printf/fprintf)
 * ============================================================================ */

static void fmt_putc_file(int ch, char **ctx) {
    fputc(ch, (FILE *)*ctx);
}

static void fmt_puts(char *s, void (*putfn)(), char **ctx) {
    while (*s) {
        putfn(*s, ctx);
        s = s + 1;
    }
}

/* 64-bit integer formatter used for %lld / %llu / %llx / %llX.  Same
 * shape as fmt_int but takes long long and a base; upper picks hex
 * case.  Decimal output ignores upper. */
static void fmt_int64(long long val, int is_unsigned, int base,
                      int width, int zero_pad, int upper,
                      void (*putfn)(), char **ctx) {
    char buf[24];
    int i;
    int neg;
    unsigned long long uval;
    int len;
    int pad;
    int d;
    char *digits;

    digits = upper ? "0123456789ABCDEF" : "0123456789abcdef";

    neg = 0;
    if (!is_unsigned && val < 0) {
        neg = 1;
        uval = (unsigned long long)(0 - val);
    } else {
        uval = (unsigned long long)val;
    }

    i = 0;
    if (uval == 0) {
        buf[i] = '0';
        i = i + 1;
    } else {
        while (uval > 0) {
            d = (int)(uval % (unsigned long long)base);
            buf[i] = digits[d];
            uval = uval / (unsigned long long)base;
            i = i + 1;
        }
    }
    len = i;
    if (neg) len = len + 1;
    pad = width - len;
    if (pad < 0) pad = 0;

    if (!zero_pad) {
        while (pad > 0) { putfn(' ', ctx); pad = pad - 1; }
    }
    if (neg) putfn('-', ctx);
    if (zero_pad) {
        while (pad > 0) { putfn('0', ctx); pad = pad - 1; }
    }
    while (i > 0) {
        i = i - 1;
        putfn(buf[i], ctx);
    }
}

static void fmt_int(int val, int is_unsigned, int base, int width, int zero_pad,
                     void (*putfn)(), char **ctx) {
    char buf[24];
    int i;
    int neg;
    int uval;
    int len;
    int pad;
    int d;

    neg = 0;
    if (!is_unsigned && val < 0) {
        neg = 1;
        uval = 0 - val;
    } else {
        uval = val;
    }

    i = 0;
    if (uval == 0) {
        buf[i] = '0';
        i = i + 1;
    } else {
        while (uval > 0) {
            d = uval % base;
            if (d < 10) buf[i] = '0' + d;
            else buf[i] = 'a' + d - 10;
            uval = uval / base;
            i = i + 1;
        }
    }
    len = i;
    if (neg) len = len + 1;

    pad = width - len;
    if (pad < 0) pad = 0;

    if (!zero_pad) {
        while (pad > 0) { putfn(' ', ctx); pad = pad - 1; }
    }
    if (neg) putfn('-', ctx);
    if (zero_pad) {
        while (pad > 0) { putfn('0', ctx); pad = pad - 1; }
    }

    while (i > 0) {
        i = i - 1;
        putfn(buf[i], ctx);
    }
}

static void fmt_hex(int val, int width, int zero_pad, int upper,
                     void (*putfn)(), char **ctx) {
    char buf[16];
    int i;
    int len;
    int pad;
    char *digits;

    if (upper) digits = "0123456789ABCDEF";
    else digits = "0123456789abcdef";

    i = 0;
    if (val == 0) {
        buf[i] = '0';
        i = i + 1;
    } else {
        while (val != 0) {
            buf[i] = digits[val & 15];
            val = (val >> 4) & 0x0FFFFFFF;
            i = i + 1;
        }
    }
    len = i;
    pad = width - len;
    if (pad < 0) pad = 0;

    if (!zero_pad) {
        while (pad > 0) { putfn(' ', ctx); pad = pad - 1; }
    }
    if (zero_pad) {
        while (pad > 0) { putfn('0', ctx); pad = pad - 1; }
    }
    while (i > 0) {
        i = i - 1;
        putfn(buf[i], ctx);
    }
}

/* fmt_core: walks the format string consuming variadic args via va_arg
 * for each specifier.  For %d/%u/%x/%X read int (or long long with ll
 * modifier); for %s/%p read pointer; for %c read int (default arg
 * promotion); for %f/%g/%e print '?' placeholder and DO NOT va_arg
 * (variadic doubles land in V/XMM regs and aren't covered by the
 * GP-only va_list path on either target — see ISSUES.md #49).
 * Precision (.N) is parsed but ignored (only %f would consume it, and
 * %f is a placeholder anyway). */
typedef char *va_list;

static int fmt_core(char *fmt, va_list ap, void (*putfn)(), char **ctx) {
    int count;
    int width;
    int zero_pad;
    int is_ll;
    int precision;
    int has_prec;

    count = 0;

    while (*fmt) {
        if (*fmt != '%') {
            putfn(*fmt, ctx);
            count = count + 1;
            fmt = fmt + 1;
            continue;
        }
        fmt = fmt + 1;

        if (*fmt == '%') {
            putfn('%', ctx);
            count = count + 1;
            fmt = fmt + 1;
            continue;
        }

        zero_pad = 0;
        if (*fmt == '0') {
            zero_pad = 1;
            fmt = fmt + 1;
        }

        width = 0;
        while (*fmt >= '0' && *fmt <= '9') {
            width = width * 10 + (*fmt - '0');
            fmt = fmt + 1;
        }

        /* Optional precision: .N (we parse but currently only float
         * specifiers would use it — those are placeholders so the
         * precision value is dropped). */
        precision = 0;
        has_prec = 0;
        if (*fmt == '.') {
            has_prec = 1;
            fmt = fmt + 1;
            while (*fmt >= '0' && *fmt <= '9') {
                precision = precision * 10 + (*fmt - '0');
                fmt = fmt + 1;
            }
        }
        (void)precision;
        (void)has_prec;

        /* Length modifier: l (long) or ll (long long).  We track only
         * the ll case — single 'l' is treated identically to no-l for
         * int-class specifiers since long == int on the s12cc subset. */
        is_ll = 0;
        if (*fmt == 'l') {
            fmt = fmt + 1;
            if (*fmt == 'l') {
                is_ll = 1;
                fmt = fmt + 1;
            }
        } else if (*fmt == 'z' || *fmt == 'j' || *fmt == 't') {
            /* Treat size_t / intmax_t / ptrdiff_t as their underlying
             * type on this build (32-bit int).  Skip the modifier. */
            fmt = fmt + 1;
        }

        if (*fmt == 'd' || *fmt == 'i') {
            if (is_ll) fmt_int64(va_arg(ap, long long), 0, 10, width, zero_pad, 0, putfn, ctx);
            else       fmt_int(va_arg(ap, int), 0, 10, width, zero_pad, putfn, ctx);
        } else if (*fmt == 'u') {
            if (is_ll) fmt_int64(va_arg(ap, long long), 1, 10, width, zero_pad, 0, putfn, ctx);
            else       fmt_int(va_arg(ap, int), 1, 10, width, zero_pad, putfn, ctx);
        } else if (*fmt == 'x') {
            if (is_ll) fmt_int64(va_arg(ap, long long), 1, 16, width, zero_pad, 0, putfn, ctx);
            else       fmt_hex(va_arg(ap, int), width, zero_pad, 0, putfn, ctx);
        } else if (*fmt == 'X') {
            if (is_ll) fmt_int64(va_arg(ap, long long), 1, 16, width, zero_pad, 1, putfn, ctx);
            else       fmt_hex(va_arg(ap, int), width, zero_pad, 1, putfn, ctx);
        } else if (*fmt == 'p') {
            fmt_puts("0x", putfn, ctx);
            fmt_int64((long long)va_arg(ap, char *), 1, 16, 0, 0, 0, putfn, ctx);
            count = count + 2;
        } else if (*fmt == 'c') {
            putfn(va_arg(ap, int), ctx);
            count = count + 1;
        } else if (*fmt == 's') {
            char *s;
            s = va_arg(ap, char *);
            if (s) {
                int slen;
                int spad;
                if (width > 0) {
                    slen = strlen(s);
                    spad = width - slen;
                    while (spad > 0) { putfn(' ', ctx); spad = spad - 1; count = count + 1; }
                }
                while (*s) {
                    putfn(*s, ctx);
                    s = s + 1;
                    count = count + 1;
                }
            } else {
                fmt_puts("(null)", putfn, ctx);
                count = count + 6;
            }
        } else if (*fmt == 'f' || *fmt == 'F' ||
                   *fmt == 'g' || *fmt == 'G' ||
                   *fmt == 'e' || *fmt == 'E') {
            /* FP placeholder — variadic doubles land in V/XMM regs and
             * are NOT covered by the GP-only va_list path on either
             * target (see ISSUES.md #49).  Print '?' so callers see
             * the gap without losing surrounding alignment, and do
             * NOT va_arg (no GP slot was consumed). */
            putfn('?', ctx);
            count = count + 1;
        } else {
            putfn('%', ctx);
            putfn(*fmt, ctx);
            count = count + 2;
        }
        fmt = fmt + 1;
    }
    return count;
}

/* True variadic now that both cc-x64 and cc-a64 implement callee-side
 * va_start / va_arg / va_end for GP types (int / long long / pointer).
 * Floating-point args still bypass the GP save area and print as '?'
 * via the placeholder in fmt_core — see ISSUES.md #49. */
int fprintf(FILE *f, char *fmt, ...) {
    va_list ap;
    char *ctx;
    int n;
    file_sys_init();
    va_start(ap, fmt);
    ctx = (char *)f;
    n = fmt_core(fmt, ap, fmt_putc_file, &ctx);
    va_end(ap);
    return n;
}

int printf(char *fmt, ...) {
    va_list ap;
    char *ctx;
    int n;
    file_sys_init();
    va_start(ap, fmt);
    ctx = (char *)stdout;
    n = fmt_core(fmt, ap, fmt_putc_file, &ctx);
    va_end(ap);
    return n;
}

void perror(char *s) {
    file_sys_init();
    if (s && *s) {
        fputs(s, stderr);
        fputs(": ", stderr);
    }
    fputs("error\n", stderr);
}
