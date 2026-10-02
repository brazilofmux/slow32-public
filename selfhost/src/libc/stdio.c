/* Selfhost libc: stdio
 *
 * A stream is a descriptor and a buffer.  Output gathers in the buffer
 * and goes to the host a block at a time -- when the buffer fills, when
 * a line ends on a line-buffered stream, at fflush, fseek, fclose and
 * exit; input comes from the host a block at a time and is handed out
 * from the buffer.  stdout is line buffered, stderr is not buffered,
 * everything else is fully buffered, and setvbuf changes any of them.
 * These are the behaviours of the clang-side runtime (runtime/stdio.c),
 * which regression/run-libc-differential.sh holds this file to.
 *
 * Until 2026-10 every call here was a request to the host: fputc was a
 * write of one byte, fgetc a read of one.  The assembler, which reads
 * its source and writes its object a character at a time, made 2.7
 * million requests to assemble the compiler, and spent half its time in
 * the host's kernel.
 *
 * The fd-named functions at the end (fdputc, fdgetc, fdwrite, ...) are
 * the older face of the same thing.  The early stages' compilers had no
 * pointers to structures, so no FILE *, and file I/O was written over
 * bare descriptors under names that would not collide with the real
 * prototypes once those could be written; the tools are still built on
 * them.  Here they are the stream functions, reached by descriptor: a
 * descriptor opened by fdopen_path (or fopen) has a stream, kept in a
 * table by descriptor number, and 0, 1 and 2 are stdin, stdout and
 * stderr.  So fdputc(c, 1), putchar(c) and fputc(c, stdout) fill one
 * buffer and come out in the order they were made.  A descriptor from a
 * bare open() has no stream, and the fd functions go straight to the
 * host for it, as they always did.
 *
 * Compiled twice: by stage07 for the stage08 compiler and tools, and by
 * stage08 for the library programs link.  It keeps to what stage07
 * compiles: no initializers on structures, declarations at the head of
 * a block, initialization in __stdio_init, called from start.c.
 */

/* Low-level I/O from mmio_no_start.s */
int open(const char *path, int flags);
int close(int fd);
int read(int fd, char *buf, int count);
int write(int fd, const char *buf, int count);
int lseek(int fd, int offset, int whence);
int unlink(const char *path);
int rename(const char *oldpath, const char *newpath);
unsigned int strlen(const char *s);
char *memcpy(char *dst, const char *src, unsigned int n);
void __s32_halt(int status);

char *malloc(int size);
void free(char *ptr);

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define FLAG_READ  1
#define FLAG_WRITE 2

/* the host's open flags (runtime/stdio_impl.h has the same) */
#define O_S32_READ   1
#define O_S32_WRITE  2
#define O_S32_APPEND 4
#define O_S32_CREATE 8
#define O_S32_TRUNC  16

/* buffering, as <stdio.h> numbers it for setvbuf */
#define BUF_FULL 0
#define BUF_LINE 1
#define BUF_NONE 2

#define STDIO_BUFSZ 4096
#define FILE_POOL_SIZE 32
#define FD_TABLE_SIZE 64

/* bpos and blen say what the buffer holds:
 *   blen == 0, bpos > 0   bpos bytes written and not yet sent
 *   blen > 0              blen bytes read from the host, bpos handed out
 *   both 0                nothing */
struct _FILE {
    int fd;
    int flags;
    int error;
    int eof;
    int used;
    char *buf;
    int bsize;
    int bpos;
    int blen;
    int mode;
    int unget;      /* a character put back, or -1 */
    int ownbuf;     /* the buffer is malloc's, to be freed */
};

typedef struct _FILE FILE;

#define ATEXIT_MAX 32       /* the standard asks for at least 32 */
typedef void (*atexit_fn_t)(void);          /* a typedef: stage07 reads no array-of-function-pointer declarator */
static atexit_fn_t _atexit_fn[ATEXIT_MAX];
static int _atexit_n;

static struct _FILE _file_pool[FILE_POOL_SIZE];
static struct _FILE _stdin_file;
static struct _FILE _stdout_file;
static struct _FILE _stderr_file;
static char _stdout_buf[STDIO_BUFSZ];
static FILE *_fd_stream[FD_TABLE_SIZE];

FILE *stdin;
FILE *stdout;
FILE *stderr;

static void s_init(FILE *fp, int fd, int flags, int mode) {
    fp->fd = fd;
    fp->flags = flags;
    fp->error = 0;
    fp->eof = 0;
    fp->used = 1;
    fp->buf = (char *)0;
    fp->bsize = 0;
    fp->bpos = 0;
    fp->blen = 0;
    fp->mode = mode;
    fp->unget = -1;
    fp->ownbuf = 0;
}

void __stdio_init(void) {
    int i;
    i = 0;
    while (i < FD_TABLE_SIZE) {
        _fd_stream[i] = (FILE *)0;
        i = i + 1;
    }
    s_init(&_stdin_file, 0, FLAG_READ, BUF_FULL);
    s_init(&_stdout_file, 1, FLAG_WRITE, BUF_LINE);
    s_init(&_stderr_file, 2, FLAG_WRITE, BUF_NONE);
    /* stdout's buffer is its own: a message can be written when the heap is gone */
    _stdout_file.buf = _stdout_buf;
    _stdout_file.bsize = STDIO_BUFSZ;

    stdin  = &_stdin_file;
    stdout = &_stdout_file;
    stderr = &_stderr_file;
    _fd_stream[0] = stdin;
    _fd_stream[1] = stdout;
    _fd_stream[2] = stderr;
}

/* ---- the buffer --------------------------------------------------------- */

/* send what was written and is still here: 0, or -1 and the error is set */
static int s_flush(FILE *fp) {
    int off;
    int n;
    if (fp->blen != 0 || fp->bpos <= 0) return 0;
    off = 0;
    while (off < fp->bpos) {
        n = write(fp->fd, fp->buf + off, fp->bpos - off);
        if (n <= 0) {
            fp->error = 1;
            fp->bpos = 0;
            return -1;
        }
        off = off + n;
    }
    fp->bpos = 0;
    return 0;
}

/* 1 when the stream has a buffer to use, getting one the first time; an
 * unbuffered stream, or no memory for a buffer, is 0 and goes to the host
 * directly */
static int s_buffer(FILE *fp) {
    if (fp->mode == BUF_NONE) return 0;
    if (fp->buf) return 1;
    fp->buf = malloc(STDIO_BUFSZ);
    if (!fp->buf) {
        fp->mode = BUF_NONE;
        return 0;
    }
    fp->bsize = STDIO_BUFSZ;
    fp->ownbuf = 1;
    fp->bpos = 0;
    fp->blen = 0;
    return 1;
}

/* The buffer holds input, or a character was put back, and the stream is
 * now written (or its position asked for from the host): forget the
 * input, and put the host's position back over what the program had not
 * read -- the host is that far ahead.  Returns how far that was. */
static int s_drop_input(FILE *fp) {
    int back;
    back = 0;
    if (fp->blen > 0) back = fp->blen - fp->bpos;
    if (fp->unget >= 0) back = back + 1;
    if (fp->blen > 0) {
        fp->blen = 0;
        fp->bpos = 0;
    }
    fp->unget = -1;
    return back;
}

/* What is waiting in stdout is sent before input is taken from stdin, so
 * a prompt with no newline is seen before its answer is awaited.  On
 * every read of stdin, not only the ones that go to the host: how far
 * the library reads ahead is then no part of what a program prints. */
static void s_before_input(FILE *fp) {
    if (fp == stdin && stdout->blen == 0 && stdout->bpos > 0) s_flush(stdout);
}

/* n bytes out; returns how many were taken (n, short of an error) */
static int s_write(FILE *fp, const char *p, int n) {
    int done;
    int room;
    int k;
    int back;
    if (n <= 0) return 0;
    if (fp->blen > 0 || fp->unget >= 0) {
        back = s_drop_input(fp);
        if (back > 0) lseek(fp->fd, 0 - back, SEEK_CUR);
    }
    done = 0;
    if (!s_buffer(fp)) {
        while (done < n) {
            k = write(fp->fd, p + done, n - done);
            if (k <= 0) {
                fp->error = 1;
                return done;
            }
            done = done + k;
        }
        return done;
    }
    while (done < n) {
        if (fp->bpos == 0 && n - done >= fp->bsize) {
            /* a buffer's worth or more, and nothing waiting: straight out */
            k = write(fp->fd, p + done, n - done);
            if (k <= 0) {
                fp->error = 1;
                return done;
            }
            done = done + k;
            continue;
        }
        room = fp->bsize - fp->bpos;
        k = n - done;
        if (k > room) k = room;
        memcpy(fp->buf + fp->bpos, p + done, (unsigned int)k);
        fp->bpos = fp->bpos + k;
        done = done + k;
        if (fp->bpos == fp->bsize) {
            if (s_flush(fp) < 0) return done - k;
        }
    }
    if (fp->mode == BUF_LINE) {
        k = 0;
        while (k < n) {
            if (p[k] == 10) {
                if (s_flush(fp) < 0) return 0;
                break;
            }
            k = k + 1;
        }
    }
    return done;
}

/* n bytes in; returns how many came (fewer at the end of the file, or on
 * an error) */
static int s_read(FILE *fp, char *p, int n) {
    int done;
    int avail;
    int k;
    done = 0;
    if (n <= 0) return 0;
    s_before_input(fp);
    if (fp->unget >= 0) {
        p[0] = fp->unget;
        fp->unget = -1;
        done = 1;
    }
    if (fp->blen == 0 && fp->bpos > 0) s_flush(fp);     /* what was written goes first */
    while (done < n) {
        avail = fp->blen - fp->bpos;
        if (avail > 0) {
            k = n - done;
            if (k > avail) k = avail;
            memcpy(p + done, fp->buf + fp->bpos, (unsigned int)k);
            fp->bpos = fp->bpos + k;
            done = done + k;
            continue;
        }
        if (!s_buffer(fp) || n - done >= fp->bsize) {
            /* no buffer, or a buffer's worth or more wanted: straight in */
            fp->blen = 0;
            fp->bpos = 0;
            k = read(fp->fd, p + done, n - done);
            if (k <= 0) {
                if (k == 0) fp->eof = 1;
                else fp->error = 1;
                break;
            }
            done = done + k;
            continue;
        }
        k = read(fp->fd, fp->buf, fp->bsize);
        fp->bpos = 0;
        if (k <= 0) {
            fp->blen = 0;
            if (k == 0) fp->eof = 1;
            else fp->error = 1;
            break;
        }
        fp->blen = k;
    }
    return done;
}

/* where the program is in the file, the buffer counted; -1 on an error */
static int s_tell(FILE *fp) {
    int pos;
    pos = lseek(fp->fd, 0, SEEK_CUR);
    if (pos < 0) return -1;
    if (fp->blen == 0) pos = pos + fp->bpos;
    else pos = pos - (fp->blen - fp->bpos);
    if (fp->unget >= 0) pos = pos - 1;
    return pos;
}

/* move; returns the host's answer, the resulting offset, or below zero */
static int s_seek(FILE *fp, int offset, int whence) {
    int rc;
    if (s_flush(fp) < 0) return -1;
    /* from here means from where the program is, and the host is past
     * whatever was read ahead */
    if (whence == SEEK_CUR) offset = offset - s_drop_input(fp);
    else s_drop_input(fp);
    rc = lseek(fp->fd, offset, whence);
    if (rc < 0) {
        fp->error = 1;
        return rc;
    }
    fp->eof = 0;
    return rc;
}

/* ---- streams: the pool and the table by descriptor ----------------------- */

static FILE *pool_alloc(void) {
    int i;
    i = 0;
    while (i < FILE_POOL_SIZE) {
        if (!_file_pool[i].used) {
            _file_pool[i].used = 1;
            return &_file_pool[i];
        }
        i = i + 1;
    }
    return (FILE *)0;
}

/* the stream of an open descriptor, or 0 when it has none */
static FILE *s_of(int fd) {
    if (fd < 0 || fd >= FD_TABLE_SIZE) return (FILE *)0;
    return _fd_stream[fd];
}

/* a stream for a descriptor just opened.  One still in the table under
 * that number belongs to a descriptor closed behind the library's back
 * (a bare close): its buffer is the dead file's, and is dropped. */
static FILE *s_attach(int fd, int flags) {
    FILE *fp;
    fp = s_of(fd);
    if (fp) {
        if (fp->ownbuf && fp->buf) free(fp->buf);
    } else {
        fp = pool_alloc();
        if (!fp) return (FILE *)0;
    }
    s_init(fp, fd, flags, BUF_FULL);
    if (fd >= 0 && fd < FD_TABLE_SIZE) _fd_stream[fd] = fp;
    return fp;
}

/* send what is waiting, close the descriptor, give the stream back */
static int s_close(FILE *fp) {
    int rc;
    int fd;
    rc = s_flush(fp);
    fd = fp->fd;
    if (close(fd) < 0) rc = -1;
    if (fp->ownbuf && fp->buf) free(fp->buf);
    if (fd >= 0 && fd < FD_TABLE_SIZE && _fd_stream[fd] == fp) _fd_stream[fd] = (FILE *)0;
    if (fp == stdin || fp == stdout || fp == stderr) {
        /* a standard stream stays what it is, closed: nothing more is buffered for it */
        fp->buf = (char *)0;
        fp->bsize = 0;
        fp->bpos = 0;
        fp->blen = 0;
        fp->unget = -1;
        fp->ownbuf = 0;
        fp->mode = BUF_NONE;
    } else {
        fp->used = 0;
        fp->fd = -1;
        fp->buf = (char *)0;
    }
    return rc;
}

static void s_flush_all(void) {
    int i;
    if (stdout) s_flush(stdout);
    if (stderr) s_flush(stderr);
    i = 0;
    while (i < FILE_POOL_SIZE) {
        if (_file_pool[i].used) s_flush(&_file_pool[i]);
        i = i + 1;
    }
}

/* r, w, a, each with or without +: the stream's directions and the host's flags */
static int s_mode(const char *mode, int *oflags) {
    int flags;
    int plus;
    int i;
    plus = 0;
    i = 1;
    while (mode[0] && mode[i]) {
        if (mode[i] == '+') plus = 1;
        i = i + 1;
    }
    if (mode[0] == 'r') {
        flags = FLAG_READ;
        *oflags = O_S32_READ;
    } else if (mode[0] == 'w') {
        flags = FLAG_WRITE;
        *oflags = O_S32_WRITE | O_S32_CREATE | O_S32_TRUNC;
    } else if (mode[0] == 'a') {
        flags = FLAG_WRITE;
        *oflags = O_S32_WRITE | O_S32_CREATE | O_S32_APPEND;
    } else {
        return 0;
    }
    if (plus) {
        flags = FLAG_READ | FLAG_WRITE;
        *oflags = *oflags | O_S32_READ | O_S32_WRITE;
    }
    return flags;
}

/* ---- <stdio.h> ----------------------------------------------------------- */

FILE *fopen(const char *path, const char *mode) {
    FILE *fp;
    int flags;
    int oflags;
    int fd;

    oflags = 0;
    flags = s_mode(mode, &oflags);
    if (!flags) return (FILE *)0;
    fd = open(path, oflags);
    if (fd < 0) return (FILE *)0;
    fp = s_attach(fd, flags);
    if (!fp) {
        close(fd);
        return (FILE *)0;
    }
    /* "a": every write goes to the end, so that is where the stream is */
    if (mode[0] == 'a' && !(flags & FLAG_READ)) lseek(fd, 0, SEEK_END);
    return fp;
}

/* a stream on a descriptor already open.  0, 1 and 2 have theirs. */
FILE *fdopen(int fd, const char *mode) {
    FILE *fp;
    int flags;
    int oflags;

    oflags = 0;
    flags = s_mode(mode, &oflags);
    if (!flags || fd < 0) return (FILE *)0;
    if (fd <= 2) {
        fp = s_of(fd);
        if (fp) return fp;
    }
    fp = s_attach(fd, flags);
    if (!fp) return (FILE *)0;
    if (mode[0] == 'a' && !(flags & FLAG_READ)) lseek(fd, 0, SEEK_END);
    return fp;
}

/* the same stream on another file: what it held is sent, its file is
 * closed, and the stream begins again on the new one.  When the new
 * file does not open the stream is closed. */
FILE *freopen(const char *path, const char *mode, FILE *fp) {
    int flags;
    int oflags;
    int fd;
    int old;
    int std;

    if (!fp) return (FILE *)0;
    std = (fp == stdin || fp == stdout || fp == stderr);
    oflags = 0;
    flags = 0;
    if (mode) flags = s_mode(mode, &oflags);
    s_flush(fp);
    old = fp->fd;
    if (old >= 0) {
        close(old);
        if (!std && old < FD_TABLE_SIZE && _fd_stream[old] == fp) _fd_stream[old] = (FILE *)0;
    }
    if (fp->ownbuf && fp->buf) free(fp->buf);
    fd = -1;
    if (flags && path) fd = open(path, oflags);
    if (fd < 0) {
        if (std) {
            s_init(fp, -1, 0, BUF_NONE);
        } else {
            fp->used = 0;
            fp->fd = -1;
            fp->buf = (char *)0;
        }
        return (FILE *)0;
    }
    s_init(fp, fd, flags, BUF_FULL);
    if (fp == stdout) {
        fp->buf = _stdout_buf;
        fp->bsize = STDIO_BUFSZ;
    }
    /* a standard stream is still found under 0, 1 or 2, where the fd
     * functions and the table look for it; and under its new number */
    if (fd < FD_TABLE_SIZE) _fd_stream[fd] = fp;
    if (mode[0] == 'a' && !(flags & FLAG_READ)) lseek(fd, 0, SEEK_END);
    return fp;
}

int fflush(FILE *fp) {
    if (!fp) {
        s_flush_all();
        return 0;
    }
    return s_flush(fp);
}

int fclose(FILE *fp) {
    if (!fp) return -1;
    return s_close(fp);
}

int fputc(int c, FILE *fp) {
    char ch;
    /* room in the buffer, nothing read ahead, and no line to end: store it */
    if (fp->buf && fp->blen == 0 && fp->unget < 0 && fp->bpos < fp->bsize - 1 &&
        (fp->mode == BUF_FULL || (fp->mode == BUF_LINE && c != 10))) {
        fp->buf[fp->bpos] = c;
        fp->bpos = fp->bpos + 1;
        return c & 255;
    }
    ch = c;
    if (s_write(fp, &ch, 1) != 1) return -1;
    return c & 255;
}

int fgetc(FILE *fp) {
    char ch;
    int c;
    if (fp->unget < 0 && fp->bpos < fp->blen) {
        if (fp == stdin) s_before_input(fp);
        c = fp->buf[fp->bpos] & 255;
        fp->bpos = fp->bpos + 1;
        return c;
    }
    if (s_read(fp, &ch, 1) != 1) return -1;
    return ch & 255;
}

int getc(FILE *fp) {
    return fgetc(fp);
}

int putc(int c, FILE *fp) {
    return fputc(c, fp);
}

int putchar(int c) {
    return fputc(c, stdout);
}

int getchar(void) {
    return fgetc(stdin);
}

unsigned int fwrite(const char *ptr, unsigned int size, unsigned int nmemb, FILE *fp) {
    unsigned int total;
    int n;
    total = size * nmemb;
    if (total == 0) return 0;
    n = s_write(fp, ptr, (int)total);
    return (unsigned int)n / size;
}

unsigned int fread(char *ptr, unsigned int size, unsigned int nmemb, FILE *fp) {
    unsigned int total;
    int n;
    total = size * nmemb;
    if (total == 0) return 0;
    n = s_read(fp, ptr, (int)total);
    return (unsigned int)n / size;
}

long ftell(FILE *fp) {
    if (!fp) return -1;
    return (long)s_tell(fp);
}

int fseek(FILE *fp, long offset, int whence) {
    if (!fp) return -1;
    if (s_seek(fp, (int)offset, whence) < 0) return -1;
    return 0;
}

void rewind(FILE *fp) {
    if (!fp) return;
    fseek(fp, 0, SEEK_SET);
    fp->error = 0;
    fp->eof = 0;
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
    int len;
    len = (int)strlen(s);
    if (s_write(fp, s, len) != len) return -1;
    return 0;
}

int puts(const char *s) {
    if (fputs(s, stdout) < 0) return -1;
    if (fputc('\n', stdout) < 0) return -1;
    return 0;
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

/* one character back: the next read gets it first */
int ungetc(int c, FILE *fp) {
    if (c == -1 || !fp || fp->unget >= 0) return -1;
    fp->unget = c & 255;
    fp->eof = 0;
    return c & 255;
}

/* before the stream is used: its buffering, and a buffer of the caller's */
int setvbuf(FILE *fp, char *buf, int mode, unsigned int size) {
    if (!fp) return -1;
    if (mode != BUF_FULL && mode != BUF_LINE && mode != BUF_NONE) return -1;
    if (s_flush(fp) < 0) return -1;
    if (fp->blen > 0 || fp->unget >= 0) return -1;      /* input already buffered: too late */
    fp->mode = mode;
    if (mode == BUF_NONE || (buf && size > 0)) {
        if (fp->ownbuf && fp->buf) free(fp->buf);
        fp->buf = (char *)0;
        fp->bsize = 0;
        fp->ownbuf = 0;
    }
    if (mode != BUF_NONE && buf && size > 0) {
        fp->buf = buf;
        fp->bsize = (int)size;
    }
    return 0;
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

/* what is buffered goes out, then the run ends with the status the
 * program gave (mmio_no_start.s: __s32_halt puts it where the host reads
 * it; the bare halt this replaces left there whatever the last call had
 * returned) */
void exit(int status) {
    atexit_fn_t fn;
    /* the last registered first; one that registers another has it run next */
    while (_atexit_n > 0) {
        _atexit_n = _atexit_n - 1;
        fn = _atexit_fn[_atexit_n];
        fn();
    }
    s_flush_all();
    __s32_halt(status);
}

/* the functions exit calls on the way out, in the reverse of this order */
int atexit(atexit_fn_t fn) {
    if (_atexit_n >= ATEXIT_MAX) return -1;
    _atexit_fn[_atexit_n] = fn;
    _atexit_n = _atexit_n + 1;
    return 0;
}

/* the run ends here and now: no atexit functions, nothing flushed */
void _exit(int status) {
    __s32_halt(status);
}

void _Exit(int status) {
    __s32_halt(status);
}

/* === fd-based I/O functions (for code that uses int fd instead of FILE*) ===
 * The stream functions by descriptor; a descriptor with no stream goes
 * to the host directly. */

int fdopen_path(const char *path, const char *mode) {
    int flags;
    int oflags;
    int fd;
    oflags = 0;
    flags = 0;
    if (mode[0] == 'r') { flags = FLAG_READ; oflags = 0x01; }          /* O_RDONLY */
    else if (mode[0] == 'w') { flags = FLAG_WRITE; oflags = 0x1A; }    /* O_WRONLY|O_CREAT|O_TRUNC */
    else if (mode[0] == 'a') { flags = FLAG_WRITE; oflags = 0x0E; }    /* O_WRONLY|O_CREAT|O_APPEND */
    else return -1;
    fd = open(path, oflags);
    if (fd >= 0) s_attach(fd, flags);       /* no stream to be had: the descriptor works unbuffered */
    return fd;
}

int fdclose(int fd) {
    FILE *fp;
    fp = s_of(fd);
    if (fp) return s_close(fp);
    return close(fd);
}

int fdputc(int c, int fd) {
    FILE *fp;
    char ch;
    fp = s_of(fd);
    if (fp) {
        fputc(c, fp);
        return c;
    }
    ch = c;
    write(fd, &ch, 1);
    return c;
}

int fdputs(const char *s, int fd) {
    FILE *fp;
    unsigned int len;
    len = strlen(s);
    fp = s_of(fd);
    if (fp) s_write(fp, s, (int)len);
    else write(fd, s, (int)len);
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
    FILE *fp;
    char ch;
    int n;
    fp = s_of(fd);
    if (fp) return fgetc(fp);
    n = read(fd, &ch, 1);
    if (n <= 0) return -1;
    return ch & 255;
}

int fdread(const char *buf, int sz, int count, int fd) {
    FILE *fp;
    int total;
    int n;
    total = sz * count;
    if (total <= 0) return 0;
    fp = s_of(fd);
    if (fp) n = s_read(fp, (char *)buf, total);
    else n = read(fd, (char *)buf, total);
    if (n <= 0) return 0;
    return n / sz;
}

int fdwrite(const char *buf, int sz, int count, int fd) {
    FILE *fp;
    int total;
    int n;
    total = sz * count;
    if (total <= 0) return 0;
    fp = s_of(fd);
    if (fp) n = s_write(fp, buf, total);
    else n = write(fd, buf, total);
    if (n <= 0) return 0;
    return n / sz;
}

/* NOTE: lseek semantics -- returns the RESULTING OFFSET, not 0/-1 like
 * fseek.  Callers must test `< 0` for failure.  Testing `!= 0` treats
 * every non-empty file as an error, which silently broke slow32dis,
 * slow32dump and every read path of s32-ar for as long as they have
 * existed. */
int fdseek(int fd, int off, int whence) {
    FILE *fp;
    fp = s_of(fd);
    if (fp) return s_seek(fp, off, whence);
    return lseek(fd, off, whence);
}

int fdtell(int fd) {
    FILE *fp;
    fp = s_of(fd);
    if (fp) return s_tell(fp);
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
