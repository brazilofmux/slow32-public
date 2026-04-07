/* Selfhost bootstrap libc: minimal stdio
 *
 * Provides FILE-based I/O for the selfhost assembler.
 * Uses a static FILE pool (no malloc needed).
 * All I/O is unbuffered, wrapping mmio_minimal.s primitives.
 *
 * Written for cc.fth subset-C compatibility:
 *   - No struct initializers
 *   - No address-of in global initializers
 *   - Initialization done via __stdio_init() called from start.c
 */

/* Low-level I/O from mmio_minimal.s */
int open(const char *path, int flags);
int close(int fd);
int read(int fd, char *buf, int count);
int write(int fd, const char *buf, int count);
int lseek(int fd, int offset, int whence);
int unlink(const char *path);
int rename(const char *oldpath, const char *newpath);
unsigned int strlen(const char *s);

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define FLAG_READ  1
#define FLAG_WRITE 2

#define FILE_POOL_SIZE 16

struct _FILE {
    int fd;
    int flags;
    int error;
    int eof;
    int used;
};

typedef struct _FILE FILE;

static struct _FILE _file_pool[FILE_POOL_SIZE];
static struct _FILE _stdin_file;
static struct _FILE _stdout_file;
static struct _FILE _stderr_file;

FILE *stdin;
FILE *stdout;
FILE *stderr;

void __stdio_init(void) {
    _stdin_file.fd = 0;
    _stdin_file.flags = FLAG_READ;
    _stdin_file.error = 0;
    _stdin_file.eof = 0;
    _stdin_file.used = 1;

    _stdout_file.fd = 1;
    _stdout_file.flags = FLAG_WRITE;
    _stdout_file.error = 0;
    _stdout_file.eof = 0;
    _stdout_file.used = 1;

    _stderr_file.fd = 2;
    _stderr_file.flags = FLAG_WRITE;
    _stderr_file.error = 0;
    _stderr_file.eof = 0;
    _stderr_file.used = 1;

    stdin  = &_stdin_file;
    stdout = &_stdout_file;
    stderr = &_stderr_file;
}

static FILE *pool_alloc(void) {
    int i;
    for (i = 0; i < FILE_POOL_SIZE; i = i + 1) {
        if (!_file_pool[i].used) {
            _file_pool[i].used = 1;
            _file_pool[i].error = 0;
            _file_pool[i].eof = 0;
            return &_file_pool[i];
        }
    }
    return (FILE *)0;
}

static void pool_free(FILE *fp) {
    fp->used = 0;
    fp->fd = -1;
}

FILE *fopen(const char *path, const char *mode) {
    FILE *fp;
    int flags;
    int fd;

    fp = pool_alloc();
    if (!fp) return (FILE *)0;

    flags = 0;
    if (*mode == 'r') {
        flags = 0;  /* O_RDONLY */
    } else if (*mode == 'w') {
        flags = 26; /* O_WRONLY | O_CREAT | O_TRUNC */
    } else if (*mode == 'a') {
        flags = 9;  /* O_WRONLY | O_APPEND */
    } else {
        pool_free(fp);
        return (FILE *)0;
    }

    fd = open(path, flags);
    if (fd < 0) {
        pool_free(fp);
        return (FILE *)0;
    }

    fp->fd = fd;
    if (*mode == 'r') {
        fp->flags = FLAG_READ;
    } else {
        fp->flags = FLAG_WRITE;
    }
    return fp;
}

int fclose(FILE *fp) {
    int rc;
    if (!fp) return -1;
    rc = close(fp->fd);
    if (fp != stdin && fp != stdout && fp != stderr) {
        pool_free(fp);
    }
    return rc;
}

int fputc(int c, FILE *fp) {
    char ch;
    int n;
    ch = (char)c;
    n = write(fp->fd, &ch, 1);
    if (n != 1) {
        fp->error = 1;
        return -1;
    }
    return c;
}

int getc(FILE *fp) {
    return fgetc(fp);
}

int putc(int c, FILE *fp) {
    return fputc(c, fp);
}

unsigned int fwrite(const char *ptr, unsigned int size, unsigned int nmemb, FILE *fp) {
    unsigned int total;
    int n;
    total = size * nmemb;
    if (total == 0) return 0;
    n = write(fp->fd, ptr, (int)total);
    if (n < 0) {
        fp->error = 1;
        return 0;
    }
    return (unsigned int)n / size;
}

unsigned int fread(char *ptr, unsigned int size, unsigned int nmemb, FILE *fp) {
    unsigned int total;
    int n;
    total = size * nmemb;
    if (total == 0) return 0;
    n = read(fp->fd, ptr, (int)total);
    if (n < 0) {
        fp->error = 1;
        return 0;
    }
    if (n == 0) {
        fp->eof = 1;
        return 0;
    }
    return (unsigned int)n / size;
}

long ftell(FILE *fp) {
    if (!fp) return -1;
    return (long)lseek(fp->fd, 0, SEEK_CUR);
}

int fseek(FILE *fp, long offset, int whence) {
    int rc;
    if (!fp) return -1;
    rc = lseek(fp->fd, (int)offset, whence);
    if (rc < 0) {
        fp->error = 1;
        return -1;
    }
    fp->eof = 0;
    return 0;
}

int fgetc(FILE *fp) {
    char c;
    int n;
    n = read(fp->fd, &c, 1);
    if (n <= 0) {
        if (n == 0) fp->eof = 1;
        else fp->error = 1;
        return -1;
    }
    return c & 255;
}

char *fgets(char *buf, int n, FILE *fp) {
    int i;
    int c;
    if (!buf || n <= 0) return (char *)0;
    i = 0;
    while (i < n - 1) {
        c = fgetc(fp);
        if (c == -1) {
            if (i == 0) return (char *)0;
            break;
        }
        buf[i] = (char)c;
        i = i + 1;
        if (c == '\n') break;
    }
    buf[i] = 0;
    return buf;
}

int fputs(const char *s, FILE *fp) {
    unsigned int len;
    int n;
    len = strlen(s);
    n = write(fp->fd, s, (int)len);
    if (n < 0) return -1;
    return 0;
}

int puts(const char *s) {
    if (fputs(s, stdout) < 0) return -1;
    if (fputc('\n', stdout) < 0) return -1;
    return 0;
}

int fflush(FILE *fp) {
    (void)fp;
    return 0; /* unbuffered */
}

void rewind(FILE *fp) {
    if (!fp) return;
    fseek(fp, 0, SEEK_SET);
    fp->error = 0;
    fp->eof = 0;
}

int feof(FILE *fp) {
    if (!fp) return 0;
    return fp->eof;
}

int ferror(FILE *fp) {
    if (!fp) return 0;
    return fp->error;
}

void clearerr(FILE *fp) {
    if (!fp) return;
    fp->error = 0;
    fp->eof = 0;
}

int fileno(FILE *fp) {
    if (!fp) return -1;
    return fp->fd;
}

void perror(const char *s) {
    if (s && *s) {
        fputs(s, stderr);
        fputs(": ", stderr);
    }
    fputs("error\n", stderr);
}

int ungetc(int c, FILE *fp) {
    (void)fp;
    return c; /* minimal stub */
}

int setvbuf(FILE *fp, char *buf, int mode, unsigned int size) {
    (void)fp;
    (void)buf;
    (void)mode;
    (void)size;
    return 0; /* unbuffered runtime ignores */
}

FILE *tmpfile(void) {
    return (FILE *)0;
}

int remove(const char *path) {
    return unlink(path);
}

void fput_uint(FILE *fp, unsigned int val) {
    char buf[11];
    int i;
    i = 0;
    if (val == 0) {
        fputc('0', fp);
        return;
    }
    while (val > 0) {
        buf[i] = '0' + (int)(val % 10);
        val = val / 10;
        i = i + 1;
    }
    while (i > 0) {
        i = i - 1;
        fputc((int)buf[i], fp);
    }
}

/* === fd-based I/O functions (for code that uses int fd instead of FILE*) === */

int fdopen_path(const char *path, const char *mode) {
    int flags;
    flags = 0;
    if (mode[0] == 'r') flags = 0x01;        /* O_RDONLY */
    else if (mode[0] == 'w') flags = 0x1A;   /* O_WRONLY|O_CREAT|O_TRUNC */
    else if (mode[0] == 'a') flags = 0x0E;   /* O_WRONLY|O_CREAT|O_APPEND */
    else return -1;
    return open(path, flags);
}

int fdclose(int fd) {
    return close(fd);
}

int fdputc(int c, int fd) {
    char ch;
    ch = c;
    write(fd, &ch, 1);
    return c;
}

int fdputs(const char *s, int fd) {
    unsigned int len;
    len = strlen(s);
    write(fd, s, (int)len);
    return 0;
}

void fdputuint(int fd, unsigned int val) {
    char buf[11];
    int i;
    i = 0;
    if (val == 0) {
        fdputc('0', fd);
        return;
    }
    while (val > 0) {
        buf[i] = '0' + (int)(val % 10);
        val = val / 10;
        i = i + 1;
    }
    while (i > 0) {
        i = i - 1;
        fdputc((int)buf[i], fd);
    }
}

int fdgetc(int fd) {
    char ch;
    int n;
    n = read(fd, &ch, 1);
    if (n <= 0) return -1;
    return ch & 255;
}

int fdread(const char *buf, int sz, int count, int fd) {
    int total;
    int n;
    total = sz * count;
    if (total <= 0) return 0;
    n = read(fd, (char *)buf, total);
    if (n <= 0) return 0;
    return n / sz;
}

int fdwrite(const char *buf, int sz, int count, int fd) {
    int total;
    int n;
    total = sz * count;
    if (total <= 0) return 0;
    n = write(fd, buf, total);
    if (n <= 0) return 0;
    return n / sz;
}

int fdseek(int fd, int off, int whence) {
    return lseek(fd, off, whence);
}

int fdtell(int fd) {
    return lseek(fd, 0, 1);
}

char *fdgets(char *buf, int n, int fd) {
    int i;
    int c;
    if (n <= 0) return (char *)0;
    i = 0;
    while (i < n - 1) {
        c = fdgetc(fd);
        if (c < 0) {
            if (i == 0) return (char *)0;
            break;
        }
        buf[i] = c;
        i = i + 1;
        if (c == 10) break;
    }
    buf[i] = 0;
    return buf;
}
