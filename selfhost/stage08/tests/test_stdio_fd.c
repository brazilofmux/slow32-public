/* The fd-named I/O functions over the buffered streams (libc/stdio.c).
 *
 * The tools are built on fdopen_path / fdputc / fdgetc / fdwrite /
 * fdread / fdseek / fdtell, and each call used to be a request to the
 * host.  They are the stream functions now, reached by descriptor.  This
 * builds a file and an array together with random operations -- single
 * characters, strings, numbers, blocks either side of the 4096-byte
 * buffer, seeks that overwrite in place -- then walks the file with
 * random reads and seeks and must find the array: every count, every
 * position (fdtell, and fdseek's returned offset), every byte.  Then the
 * things only two faces of one stream can get wrong: a FILE and its
 * descriptor written through alternately, a descriptor from a bare
 * open() (no stream: straight to the host) and a descriptor number
 * reused after a bare close().
 *
 * Returns 0, or the number of the first check that failed. */
#include <stdio.h>
#include <string.h>

int open(const char *path, int flags);
int close(int fd);
int write(int fd, const char *buf, int count);
int unlink(const char *path);

#define MAXLEN 300000
static char model[MAXLEN];
static int mlen;
static unsigned int seed;
static char buf[12000];
static int fail_at;

static unsigned int rnd(void) {
    seed = seed * 1103515245u + 12345u;
    return (seed >> 8) & 0xFFFFFF;
}
static void bad(int n) { if (!fail_at) fail_at = n; }
static void put(int at, const char *p, int n) {
    if (at + n > MAXLEN) { bad(90); return; }
    memcpy(model + at, p, (unsigned int)n);
    if (at + n > mlen) mlen = at + n;
}
static int size_of(int k) {
    static int sizes[14];
    sizes[0] = 1; sizes[1] = 2; sizes[2] = 3; sizes[3] = 7; sizes[4] = 80; sizes[5] = 255; sizes[6] = 1000;
    sizes[7] = 4094; sizes[8] = 4095; sizes[9] = 4096; sizes[10] = 4097; sizes[11] = 5000; sizes[12] = 9000; sizes[13] = 1;
    return sizes[k % 14];
}
static void fill(int n) {
    int i;
    for (i = 0; i < n; i++) buf[i] = (char)(rnd() >> 3);
}

static void writes(int fd, int ops, int limit) {
    int k; int c; int n; int i; int pos; unsigned int v; char num[12]; int nd; int r;
    pos = mlen;
    for (k = 0; k < ops; k++) {
        c = (int)(rnd() % 10);
        n = size_of((int)rnd());
        if (mlen + 9100 > limit) break;
        if (c < 3) {                            /* single characters */
            n = n % 200 + 1;
            for (i = 0; i < n; i++) {
                buf[0] = (char)rnd();
                if (fdputc(buf[0], fd) != buf[0]) bad(1);
                put(pos, buf, 1); pos = pos + 1;
            }
        } else if (c == 3) {                    /* a string */
            n = n % 150;
            for (i = 0; i < n; i++) buf[i] = (char)('a' + rnd() % 26);
            buf[n] = 0;
            fdputs(buf, fd);
            put(pos, buf, n); pos = pos + n;
        } else if (c == 4) {                    /* a number */
            v = rnd() * 7u;
            fdputuint(fd, v);
            nd = 0;
            if (v == 0) { num[0] = '0'; nd = 1; }
            while (v > 0) { num[nd] = (char)('0' + v % 10); v = v / 10; nd = nd + 1; }
            for (i = 0; i < nd; i++) buf[i] = num[nd - 1 - i];
            put(pos, buf, nd); pos = pos + nd;
        } else if (c < 8) {                     /* a block */
            fill(n);
            r = fdwrite(buf, 1, n, fd);
            if (r != n) bad(2);
            put(pos, buf, n); pos = pos + n;
        } else if (c == 8) {                    /* where are we */
            if (fdtell(fd) != pos) bad(3);
        } else {                                /* back into what is written, and over it */
            if (mlen > 0) {
                pos = (int)(rnd() % (unsigned int)mlen);
                if (fdseek(fd, pos, 0) != pos) bad(4);
            }
        }
    }
    if (fdseek(fd, 0, 2) != mlen) bad(5);       /* the end is where the model ends */
}

static void reads(int fd, int ops) {
    int k; int c; int n; int i; int pos; int left; int want; int r; int d;
    pos = 0;
    for (k = 0; k < ops; k++) {
        c = (int)(rnd() % 10);
        n = size_of((int)rnd());
        left = mlen - pos;
        if (c < 3) {
            n = n % 200 + 1;
            for (i = 0; i < n; i++) {
                r = fdgetc(fd);
                if (pos < mlen) { if (r != (model[pos] & 255)) bad(10); pos = pos + 1; }
                else if (r != -1) bad(11);
            }
        } else if (c < 6) {
            want = n < left ? n : left;
            r = fdread(buf, 1, n, fd);
            if (r != want) bad(12);
            else if (memcmp(buf, model + pos, (unsigned int)want) != 0) bad(13);
            pos = pos + want;
        } else if (c == 6) {                    /* elements of four bytes */
            want = 4 * (n % 500 + 1);
            if (want > left) want = left;
            r = fdread(buf, 4, n % 500 + 1, fd);
            if (r != want / 4) bad(14);
            else if (memcmp(buf, model + pos, (unsigned int)want) != 0) bad(15);
            pos = pos + want;
        } else if (c == 7) {
            if (fdtell(fd) != pos) bad(16);
        } else if (c == 8) {
            pos = mlen ? (int)(rnd() % (unsigned int)(mlen + 1)) : 0;
            if (fdseek(fd, pos, 0) != pos) bad(17);
        } else {                                /* from here: the read-ahead must not count */
            d = (int)(rnd() % 9000) - 4500;
            if (d < 0 - pos) d = 0 - pos;
            if (d > mlen - pos) d = mlen - pos;
            if (fdseek(fd, d, 1) != pos + d) bad(18);
            pos = pos + d;
        }
    }
}

int main(void) {
    int fd; int fd2; int i; int r; FILE *f; char line[64]; char *p;

    seed = 4242;
    mlen = 0;
    fail_at = 0;

    fd = fdopen_path("stdio_fd.dat", "w");
    if (fd < 0) return 100;
    writes(fd, 3000, 200000);
    if (fdclose(fd) != 0) bad(20);

    fd = fdopen_path("stdio_fd.dat", "r");
    if (fd < 0) return 101;
    reads(fd, 4000);
    /* the whole file once more, in the largest reads */
    if (fdseek(fd, 0, 0) != 0) bad(21);
    i = 0;
    for (;;) {
        r = fdread(buf, 1, 12000, fd);
        if (r <= 0) break;
        if (i + r > mlen || memcmp(buf, model + i, (unsigned int)r) != 0) { bad(22); break; }
        i = i + r;
    }
    if (i != mlen) bad(23);
    if (fdgetc(fd) != -1) bad(24);
    fdclose(fd);

    /* append: the end is the start */
    fd = fdopen_path("stdio_fd.dat", "a");
    if (fd < 0) return 102;
    fill(300);
    if (fdwrite(buf, 1, 300, fd) != 300) bad(30);
    put(mlen, buf, 300);
    fdclose(fd);
    fd = fdopen_path("stdio_fd.dat", "r");
    reads(fd, 1500);
    fdclose(fd);

    /* a line at a time */
    fd = fdopen_path("stdio_fd.dat", "w");
    fdputs("first\nsecond line\n\nlast, with no newline", fd);
    fdclose(fd);
    fd = fdopen_path("stdio_fd.dat", "r");
    p = fdgets(line, 64, fd); if (!p || strcmp(line, "first\n") != 0) bad(40);
    p = fdgets(line, 8, fd);  if (!p || strcmp(line, "second ") != 0) bad(41);
    p = fdgets(line, 64, fd); if (!p || strcmp(line, "line\n") != 0) bad(42);
    p = fdgets(line, 64, fd); if (!p || strcmp(line, "\n") != 0) bad(43);
    p = fdgets(line, 64, fd); if (!p || strcmp(line, "last, with no newline") != 0) bad(44);
    p = fdgets(line, 64, fd); if (p) bad(45);
    fdclose(fd);

    /* a FILE and its descriptor are one stream: written through both in turn */
    f = fopen("stdio_fd.dat", "w");
    if (!f) return 103;
    fd = fileno(f);
    fputs("A1 ", f); fdputs("b2 ", fd); fputc('C', f); fdputc('d', fd); fwrite(" E5", 1, 3, f); fdwrite(" f6", 1, 3, fd);
    if (ftell(f) != 14 || fdtell(fd) != 14) bad(50);
    fclose(f);
    f = fopen("stdio_fd.dat", "r");
    fd = fileno(f);
    if (fgetc(f) != 'A' || fdgetc(fd) != '1' || fgetc(f) != ' ') bad(51);
    if (ungetc('#', f) != '#') bad(52);
    if (fdgetc(fd) != '#' || fdgetc(fd) != 'b') bad(53);            /* the character put back comes first, by either name */
    r = (int)fread(line, 1, 63, f); line[r] = 0;
    if (strcmp(line, "2 Cd E5 f6") != 0) bad(54);
    if (fgetc(f) != -1 || !feof(f)) bad(55);
    fclose(f);

    /* read part of the buffer's read-ahead, then write with no seek between: the write lands where
     * the reading stopped, not where the host's position is (the end of what was read ahead) */
    f = fopen("stdio_fd.dat", "w"); fputs("0123456789", f); fclose(f);
    f = fopen("stdio_fd.dat", "r+");
    fd = fileno(f);
    if (fdgetc(fd) != '0' || fgetc(f) != '1' || fdgetc(fd) != '2') bad(56);
    fdputs("XY", fd);
    if (ftell(f) != 5) bad(57);
    fclose(f);
    f = fopen("stdio_fd.dat", "r");
    r = (int)fread(line, 1, 63, f); line[r] = 0;
    fclose(f);
    if (strcmp(line, "012XY56789") != 0) bad(58);

    /* a descriptor from a bare open() has no stream: each call goes to the host, and it still works */
    fd = open("stdio_fd.dat", 26);
    if (fd < 0) return 104;
    fdputs("raw ", fd); fdputc('x', fd); write(fd, " mixed", 6); fdputuint(fd, 7u);
    close(fd);
    /* ... and the next open gets the same number: nothing of the old descriptor may remain */
    fd2 = fdopen_path("stdio_fd.dat", "r");
    r = fdread(line, 1, 63, fd2); line[r > 0 ? r : 0] = 0;
    if (strcmp(line, "raw x mixed7") != 0) bad(60);
    close(fd2);                                                 /* a bare close of a descriptor with a stream */
    fd = fdopen_path("stdio_fd.dat", "w");                      /* the number again: the stale stream is dropped */
    fdputs("fresh", fd);
    fdclose(fd);
    fd = fdopen_path("stdio_fd.dat", "r");
    r = fdread(line, 1, 63, fd); line[r > 0 ? r : 0] = 0;
    if (strcmp(line, "fresh") != 0) bad(61);
    fdclose(fd);

    /* unbuffered by request: each write is at the host at once, visible through another descriptor */
    f = fopen("stdio_fd.dat", "w");
    if (setvbuf(f, (char *)0, _IONBF, 0) != 0) bad(70);
    fputs("now", f);
    fd = fdopen_path("stdio_fd.dat", "r");
    r = fdread(line, 1, 63, fd); line[r > 0 ? r : 0] = 0;
    if (strcmp(line, "now") != 0) bad(71);
    fdclose(fd);
    fclose(f);
    /* ... and buffered, it is not there until the flush */
    f = fopen("stdio_fd.dat", "w");
    fputs("later", f);
    fd = fdopen_path("stdio_fd.dat", "r");
    if (fdgetc(fd) != -1) bad(72);
    fdclose(fd);
    if (fflush(f) != 0) bad(73);
    fd = fdopen_path("stdio_fd.dat", "r");
    r = fdread(line, 1, 63, fd); line[r > 0 ? r : 0] = 0;
    if (strcmp(line, "later") != 0) bad(74);
    fdclose(fd);
    fclose(f);

    unlink("stdio_fd.dat");
    return fail_at;
}
