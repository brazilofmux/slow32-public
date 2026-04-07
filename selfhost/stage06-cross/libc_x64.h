





#define O_RDONLY  0
#define O_WRONLY  1
#define O_RDWR   2
#define O_CREAT   64
#define O_TRUNC  512
#define O_APPEND 1024


#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2


#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2


#define NULL  0
#define EOF  (-1)


#define INT_MAX  2147483647
#define UINT_MAX 4294967295


#define true  1
#define false 0




int __syscall();



static int sys_write(int fd, char *buf, int len) {
    return __syscall(1, fd, buf, len, 0, 0, 0);
}

static int sys_read(int fd, char *buf, int len) {
    return __syscall(0, fd, buf, len, 0, 0, 0);
}

static int sys_open(char *path, int flags, int mode) {
    return __syscall(2, path, flags, mode, 0, 0, 0);
}

static int sys_close(int fd) {
    return __syscall(3, fd, 0, 0, 0, 0, 0);
}

static int sys_lseek(int fd, int offset, int whence) {
    return __syscall(8, fd, offset, whence, 0, 0, 0);
}

static int sys_stat(char *path, char *buf) {
    return __syscall(4, path, buf, 0, 0, 0, 0);
}

static int sys_fstat(int fd, char *buf) {
    return __syscall(5, fd, buf, 0, 0, 0, 0);
}

static int sys_brk(int addr) {
    return __syscall(12, addr, 0, 0, 0, 0, 0);
}

void exit(int code) {
    __syscall(60, code, 0, 0, 0, 0, 0);
}



int write(int fd, char *buf, int len) {
    return sys_write(fd, buf, len);
}

int read(int fd, char *buf, int len) {
    return sys_read(fd, buf, len);
}

int open(char *path, int flags, int mode) {
    return sys_open(path, flags, mode);
}

int close(int fd) {
    return sys_close(fd);
}

int lseek(int fd, int offset, int whence) {
    return sys_lseek(fd, offset, whence);
}



int stat(char *path, char *buf) {
    return sys_stat(path, buf);
}

int fstat(int fd, char *buf) {
    return sys_fstat(fd, buf);
}



char *memcpy(char *dst, char *src, int n) {
    int i;
    i = 0;
    while (i < n) {
        dst[i] = src[i];
        i = i + 1;
    }
    return dst;
}

char *memset(char *dst, int c, int n) {
    int i;
    i = 0;
    while (i < n) {
        dst[i] = c;
        i = i + 1;
    }
    return dst;
}

int strlen(char *s) {
    int n;
    n = 0;
    while (s[n]) n = n + 1;
    return n;
}

int strcmp(char *a, char *b) {
    while (*a && *a == *b) {
        a = a + 1;
        b = b + 1;
    }
    return (*(unsigned char *)a) - (*(unsigned char *)b);
}

int strncmp(char *a, char *b, int n) {
    int i;
    i = 0;
    while (i < n && a[i] && a[i] == b[i]) i = i + 1;
    if (i == n) return 0;
    return (*(unsigned char *)(a + i)) - (*(unsigned char *)(b + i));
}

int memcmp(char *a, char *b, int n) {
    int i;
    i = 0;
    while (i < n) {
        if (a[i] != b[i])
            return (*(unsigned char *)(a + i)) - (*(unsigned char *)(b + i));
        i = i + 1;
    }
    return 0;
}

char *strcpy(char *dst, char *src) {
    int i;
    i = 0;
    while (src[i]) {
        dst[i] = src[i];
        i = i + 1;
    }
    dst[i] = 0;
    return dst;
}

char *strncpy(char *dst, char *src, int n) {
    int i;
    i = 0;
    while (i < n && src[i]) {
        dst[i] = src[i];
        i = i + 1;
    }
    while (i < n) {
        dst[i] = 0;
        i = i + 1;
    }
    return dst;
}



static char *heap_cur;
static char *heap_end;

static void heap_init() {
    if (heap_cur) return;
    heap_cur = (char *)sys_brk(0);
    heap_end = heap_cur;
}

char *malloc(int size) {
    char *p;
    char *new_end;
    heap_init();
    
    size = (size + 15) & ~15;
    
    size = size + 16;
    if (heap_cur + size > heap_end) {
        
        int grow;
        grow = size;
        if (grow < 65536) grow = 65536;
        new_end = (char *)sys_brk((int)(heap_end + grow));
        if (new_end == heap_end) return 0; 
        heap_end = new_end;
    }
    p = heap_cur;
    
    *(int *)p = size;
    heap_cur = heap_cur + size;
    return p + 16;
}

void free(char *ptr) {
    
}

char *calloc(int n, int size) {
    int total;
    char *p;
    total = n * size;
    p = malloc(total);
    if (p) memset(p, 0, total);
    return p;
}

char *realloc(char *ptr, int size) {
    char *p;
    int old_size;
    if (!ptr) return malloc(size);
    
    old_size = *(int *)(ptr - 16) - 16;
    if (size <= old_size) return ptr;
    p = malloc(size);
    if (p) memcpy(p, ptr, old_size);
    return p;
}




#define FILE_BUF_SIZE 4096
#define MAX_FILES     32

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

static void file_sys_init() {
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

int fflush(FILE *f) {
    if (!f) return 0;
    if (f->buf && f->buf_dirty > 0) {
        sys_write(f->fd, f->buf, f->buf_dirty);
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
        flags = O_WRONLY | O_CREAT | O_TRUNC;
    } else if (mode[0] == 'a') {
        flags = O_WRONLY | O_CREAT | O_APPEND;
    } else if (mode[0] == 'r') {
        if (mode[1] == '+') flags = O_RDWR;
        else flags = O_RDONLY;
    }

    fd = sys_open(path, flags, 420); 
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
        sys_close(fd);
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
    if (f->flags & 16) sys_close(f->fd);
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
        
        sys_write(f->fd, &ch, 1);
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
        n = sys_read(f->fd, f->buf, FILE_BUF_SIZE);
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
    rc = sys_lseek(f->fd, offset, whence);
    if (rc < 0) return -1;
    return 0;
}

int ftell(FILE *f) {
    int pos;
    fflush(f);
    pos = sys_lseek(f->fd, 0, SEEK_CUR);
    
    if (f->buf_len > f->buf_pos)
        pos = pos - (f->buf_len - f->buf_pos);
    return pos;
}

int putchar(int c) {
    file_sys_init();
    return fputc(c, stdout);
}





static void fmt_putc_file(int ch, char **ctx) {
    fputc(ch, (FILE *)*ctx);
}

static void fmt_puts(char *s, void (*putfn)(), char **ctx) {
    while (*s) {
        putfn(*s, ctx);
        s = s + 1;
    }
}

static void fmt_int(int val, int is_unsigned, int base, int width, int zero_pad,
                     void (*putfn)(), char **ctx) {
    char buf[24];
    int i;
    int neg;
    unsigned int uval;
    int len;
    int pad;

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
            int d;
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

static void fmt_hex(unsigned int val, int width, int zero_pad, int upper,
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
        while (val > 0) {
            buf[i] = digits[val & 15];
            val = val >> 4;
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

static int fmt_core(char *fmt, int *args, void (*putfn)(), char **ctx) {
    int count;
    int ai;
    int width;
    int zero_pad;

    count = 0;
    ai = 0; 

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

        
        if (*fmt == 'l') fmt = fmt + 1;

        
        if (*fmt == 'd' || *fmt == 'i') {
            fmt_int(args[ai], 0, 10, width, zero_pad, putfn, ctx);
            ai = ai + 1;
        } else if (*fmt == 'u') {
            fmt_int(args[ai], 1, 10, width, zero_pad, putfn, ctx);
            ai = ai + 1;
        } else if (*fmt == 'x') {
            fmt_hex(args[ai], width, zero_pad, 0, putfn, ctx);
            ai = ai + 1;
        } else if (*fmt == 'X') {
            fmt_hex(args[ai], width, zero_pad, 1, putfn, ctx);
            ai = ai + 1;
        } else if (*fmt == 'p') {
            fmt_puts("0x", putfn, ctx);
            fmt_hex(args[ai], 0, 0, 0, putfn, ctx);
            ai = ai + 1;
            count = count + 2;
        } else if (*fmt == 'c') {
            putfn(args[ai], ctx);
            ai = ai + 1;
            count = count + 1;
        } else if (*fmt == 's') {
            if (args[ai]) {
                char *s;
                s = (char *)args[ai];
                if (width > 0) {
                    int slen;
                    int pad;
                    slen = strlen(s);
                    pad = width - slen;
                    while (pad > 0) { putfn(' ', ctx); pad = pad - 1; count = count + 1; }
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
            ai = ai + 1;
        } else {
            
            putfn('%', ctx);
            putfn(*fmt, ctx);
            count = count + 2;
        }
        fmt = fmt + 1;
    }
    return count;
}



int fprintf(FILE *f, char *fmt, int a0, int a1, int a2, int a3,
            int a4, int a5, int a6, int a7) {
    int args[8];
    char *ctx;
    file_sys_init();
    args[0] = a0;
    args[1] = a1;
    args[2] = a2;
    args[3] = a3;
    args[4] = a4;
    args[5] = a5;
    args[6] = a6;
    args[7] = a7;
    ctx = (char *)f;
    return fmt_core(fmt, args, fmt_putc_file, &ctx);
}

int printf(char *fmt, int a0, int a1, int a2, int a3,
           int a4, int a5, int a6, int a7) {
    file_sys_init();
    return fprintf(stdout, fmt, a0, a1, a2, a3, a4, a5, a6, a7);
}


void perror(char *s) {
    file_sys_init();
    if (s && *s) {
        fputs(s, stderr);
        fputs(": ", stderr);
    }
    fputs("error\n", stderr);
}




static char **saved_envp;

void __save_envp(char **envp) {
    saved_envp = envp;
}

char *getenv(char *name) {
    int nlen;
    char **ep;
    if (!saved_envp) return 0;
    nlen = strlen(name);
    ep = saved_envp;
    while (*ep) {
        if (strncmp(*ep, name, nlen) == 0 && (*ep)[nlen] == '=')
            return *ep + nlen + 1;
        ep = ep + 1;
    }
    return 0;
}



void abort() {
    write(2, "abort\n", 6);
    exit(134);
}

int atoi(char *s) {
    int n;
    int neg;
    n = 0;
    neg = 0;
    while (*s == ' ' || *s == '\t' || *s == '\n') s = s + 1;
    if (*s == '-') { neg = 1; s = s + 1; }
    else if (*s == '+') s = s + 1;
    while (*s >= '0' && *s <= '9') {
        n = n * 10 + (*s - '0');
        s = s + 1;
    }
    if (neg) return 0 - n;
    return n;
}

int abs(int x) {
    if (x < 0) return 0 - x;
    return x;
}

char *strdup(char *s) {
    int len;
    char *p;
    len = strlen(s);
    p = malloc(len + 1);
    if (p) memcpy(p, s, len + 1);
    return p;
}

int isspace(int c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v';
}

int isdigit(int c) {
    return c >= '0' && c <= '9';
}

int isalpha(int c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

int toupper(int c) {
    if (c >= 'a' && c <= 'z') return c - 32;
    return c;
}

int tolower(int c) {
    if (c >= 'A' && c <= 'Z') return c + 32;
    return c;
}
