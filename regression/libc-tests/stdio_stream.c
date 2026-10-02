/* stdio's short entries (runtime/stdio.c: fwrite, fread, fputc, fgetc in
 * front of the general routines) against a model in memory.
 *
 * A stream of random operations -- writes of one byte, of a few, of a
 * buffer's worth less one, exactly one, one more; element sizes other
 * than 1; fputc, fputs, fflush, ftell -- builds a file and an array
 * together; then random reads, seeks, fgetc, ungetc and fgets walk the
 * file and must find the array.  Every count returned is checked, every
 * position ftell reports, every byte.  The sizes straddle the 4096-byte
 * buffer on purpose: the short entry must hand over to the general
 * routine exactly where the buffer would fill, and take over again after.
 * Then the same file is appended to, and overwritten in place through
 * "r+" with a seek between reading and writing. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLEN (640 * 1024)
static unsigned char model[MAXLEN];
static unsigned int mlen;
static unsigned int seed = 12345;
static int errors;
static unsigned int rnd(void) { seed = seed * 1103515245u + 12345u; return (seed >> 8) & 0xFFFFFF; }
static void fail(const char *what, unsigned int a, unsigned int b)
{
    if (errors < 20) printf("FAIL %s: %u %u\n", what, a, b);
    errors++;
}
static const unsigned int sizes[] = { 1, 1, 1, 2, 3, 7, 16, 80, 133, 255, 1000, 4094, 4095, 4096, 4097, 5000, 9000 };
#define NSIZES (sizeof sizes / sizeof sizes[0])
static unsigned char buf[16384];

static void fill(unsigned int n) { for (unsigned int i = 0; i < n; i++) buf[i] = (unsigned char)(rnd() >> 3); }
static void put(unsigned int at, const unsigned char *p, unsigned int n)
{
    if (at + n > MAXLEN) { fail("model overflow", at, n); return; }
    memcpy(model + at, p, n);
    if (at + n > mlen) mlen = at + n;
}

static void writes(FILE *f, int ops, unsigned int limit)
{
    for (int k = 0; k < ops; k++) {
        unsigned int c = rnd() % 12, n = sizes[rnd() % NSIZES];
        if (mlen + 3 * 9000 > limit) break;
        switch (c) {
        case 0: case 1: case 2: {               /* bytes, the size 1 */
            fill(n);
            size_t r = fwrite(buf, 1, n, f);
            if (r != n) fail("fwrite(1,n)", (unsigned)r, n);
            put(mlen, buf, n);
            break;
        }
        case 3: {                               /* one element of n bytes */
            fill(n);
            size_t r = fwrite(buf, n, 1, f);
            if (r != 1) fail("fwrite(n,1)", (unsigned)r, n);
            put(mlen, buf, n);
            break;
        }
        case 4: {                               /* elements of three bytes: the general routine */
            unsigned int e = n % 700 + 2;
            fill(3 * e);
            size_t r = fwrite(buf, 3, e, f);
            if (r != e) fail("fwrite(3,e)", (unsigned)r, e);
            put(mlen, buf, 3 * e);
            break;
        }
        case 5: case 6: case 7: {               /* a run of single characters */
            unsigned int e = n % 300 + 1;
            for (unsigned int i = 0; i < e; i++) {
                unsigned char ch = (unsigned char)rnd();
                int r = fputc(ch, f);
                if (r != ch) fail("fputc", (unsigned)r, ch);
                put(mlen, &ch, 1);
            }
            break;
        }
        case 8: {                               /* a string */
            unsigned int e = n % 200;
            for (unsigned int i = 0; i < e; i++) buf[i] = (unsigned char)('a' + rnd() % 26);
            buf[e] = 0;
            if (fputs((char *)buf, f) < 0) fail("fputs", e, 0);
            put(mlen, buf, e);
            break;
        }
        case 9:
            if (fflush(f) != 0) fail("fflush", 0, 0);
            break;
        case 10: {
            long t = ftell(f);
            if ((unsigned int)t != mlen) fail("ftell while writing", (unsigned)t, mlen);
            break;
        }
        default: {                              /* nothing at all */
            if (fwrite(buf, 1, 0, f) != 0 || fwrite(buf, 0, 5, f) != 0) fail("fwrite of nothing", 0, 0);
            break;
        }
        }
    }
}

static void reads(FILE *f, int ops)
{
    unsigned int pos = 0;
    for (int k = 0; k < ops; k++) {
        unsigned int c = rnd() % 12, n = sizes[rnd() % NSIZES];
        unsigned int left = mlen - pos;
        switch (c) {
        case 0: case 1: case 2: {
            size_t r = fread(buf, 1, n, f);
            unsigned int want = n < left ? n : left;
            if (r != want) fail("fread(1,n)", (unsigned)r, want);
            else if (memcmp(buf, model + pos, want)) fail("fread(1,n) bytes", pos, want);
            pos += want;
            break;
        }
        case 3: {
            size_t r = fread(buf, n, 1, f);
            if (n <= left) {
                if (r != 1) fail("fread(n,1)", (unsigned)r, n);
                else if (memcmp(buf, model + pos, n)) fail("fread(n,1) bytes", pos, n);
                pos += n;
            } else {                            /* a partial element: none returned, the bytes consumed */
                if (r != 0) fail("fread(n,1) at the end", (unsigned)r, n);
                pos = mlen;
            }
            break;
        }
        case 4: {
            unsigned int e = n % 700 + 2, bytes = 3 * e < left ? 3 * e : left;
            size_t r = fread(buf, 3, e, f);
            if (r != bytes / 3) fail("fread(3,e)", (unsigned)r, bytes / 3);
            else if (memcmp(buf, model + pos, bytes)) fail("fread(3,e) bytes", pos, bytes);
            pos += bytes;
            break;
        }
        case 5: case 6: {
            unsigned int e = n % 300 + 1;
            for (unsigned int i = 0; i < e; i++) {
                int ch = fgetc(f);
                if (pos < mlen) { if (ch != model[pos]) fail("fgetc", (unsigned)ch, pos); pos++; }
                else if (ch != EOF) fail("fgetc at the end", (unsigned)ch, pos);
            }
            break;
        }
        case 7: {                               /* a character put back, then a read across it */
            if (pos == 0 || pos >= mlen) break;
            int ch = fgetc(f);
            if (ch != model[pos]) fail("fgetc before ungetc", (unsigned)ch, pos);
            if (ungetc(ch, f) != ch) fail("ungetc", (unsigned)ch, pos);
            unsigned int want = n < left ? n : left;
            size_t r = fread(buf, 1, n, f);
            if (r != want) fail("fread after ungetc", (unsigned)r, want);
            else if (memcmp(buf, model + pos, want)) fail("fread after ungetc bytes", pos, want);
            pos += want;
            break;
        }
        case 8: {                               /* somewhere else */
            pos = mlen ? rnd() % (mlen + 1) : 0;
            if (rnd() % 8 == 0) pos = mlen > 5000 ? mlen - rnd() % 5000 : 0;
            if (fseek(f, (long)pos, SEEK_SET) != 0) fail("fseek", pos, 0);
            break;
        }
        case 9: {                               /* a little way from here: the buffer's read-ahead must not count */
            long d = (long)(rnd() % 12000) - 6000;
            if (d < -(long)pos) d = -(long)pos;
            if (d > (long)(mlen - pos)) d = (long)(mlen - pos);
            if (fseek(f, d, SEEK_CUR) != 0) fail("fseek from here", pos, (unsigned)d);
            pos = (unsigned int)((long)pos + d);
            break;
        }
        case 10: {
            long t = ftell(f);
            if ((unsigned int)t != pos) fail("ftell while reading", (unsigned)t, pos);
            break;
        }
        default: {
            if (fread(buf, 1, 0, f) != 0 || fread(buf, 0, 5, f) != 0) fail("fread of nothing", 0, 0);
            break;
        }
        }
    }
}

static unsigned int sum(void)
{
    unsigned int s = 0;
    for (unsigned int i = 0; i < mlen; i++) s = s * 31 + model[i];
    return s;
}

int main(void)
{
    FILE *f = fopen("short_paths.dat", "w");
    if (!f) { printf("fopen w failed\n"); return 1; }
    writes(f, 4000, 440 * 1024);
    if (fclose(f) != 0) fail("fclose", 0, 0);
    printf("written: %u bytes, sum %u, errors %d\n", mlen, sum(), errors);

    f = fopen("short_paths.dat", "r");
    if (!f) { printf("fopen r failed\n"); return 1; }
    reads(f, 6000);
    fclose(f);
    printf("read back: errors %d\n", errors);

    f = fopen("short_paths.dat", "a");
    if (!f) { printf("fopen a failed\n"); return 1; }
    writes(f, 600, MAXLEN);
    if (fclose(f) != 0) fail("fclose after append", 0, 0);
    f = fopen("short_paths.dat", "r");
    reads(f, 2500);
    fclose(f);
    printf("appended: %u bytes, sum %u, errors %d\n", mlen, sum(), errors);

    /* in place: read a little, seek, overwrite, seek, read it back */
    f = fopen("short_paths.dat", "r+");
    if (!f) { printf("fopen r+ failed\n"); return 1; }
    for (int k = 0; k < 300; k++) {
        unsigned int n = sizes[rnd() % NSIZES], at = rnd() % (mlen - 20000);
        if (fseek(f, (long)at, SEEK_SET) != 0) fail("fseek r+", at, 0);
        if (fread(buf, 1, 7, f) != 7 || memcmp(buf, model + at, 7)) fail("fread r+", at, 7);
        if (fseek(f, (long)at, SEEK_SET) != 0) fail("fseek r+ back", at, 0);
        fill(n);
        if (rnd() & 1) { if (fwrite(buf, 1, n, f) != n) fail("fwrite r+", at, n); }
        else for (unsigned int i = 0; i < n; i++) if (fputc(buf[i], f) != buf[i]) fail("fputc r+", at, i);
        put(at, buf, n);
        if ((unsigned int)ftell(f) != at + n) fail("ftell r+", (unsigned)ftell(f), at + n);
    }
    fseek(f, 0, SEEK_SET);
    reads(f, 2500);
    fclose(f);
    printf("in place: %u bytes, sum %u, errors %d\n", mlen, sum(), errors);

    /* the whole file once more, by the largest reads */
    f = fopen("short_paths.dat", "r");
    unsigned int pos = 0; size_t r;
    while ((r = fread(buf, 1, sizeof buf, f)) > 0) {
        if (pos + r > mlen || memcmp(buf, model + pos, r)) { fail("final pass", pos, (unsigned)r); break; }
        pos += (unsigned int)r;
    }
    if (pos != mlen) fail("final length", pos, mlen);
    fclose(f);
    remove("short_paths.dat");
    printf("%s\n", errors ? "stdio short paths: FAILED" : "stdio short paths: all checks passed");
    return errors != 0;
}
