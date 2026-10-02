/* A stream turning from reading to writing, and SEEK_CUR on a stream
 * with read-ahead in its buffer (runtime/stdio.c).  Two defects lived
 * here until 2026-10-01:
 *  - output directly after input that met end-of-file (C allows it with
 *    no positioning call between) went into the reader's buffer and was
 *    never written: fputs after reading "abc" to the end left "abc";
 *  - fseek(f, n, SEEK_CUR) counted from the host's position, which is
 *    the end of the buffer's read-ahead, not from the reader: after one
 *    fgetc of a 100-byte file, fseek(f, 0, SEEK_CUR) went to byte 100.
 * Every line below is what a hosted C library prints. */
#include <stdio.h>
#include <string.h>

static void show(const char *what)
{
    char b[64]; FILE *f = fopen("turn.dat", "r");
    size_t r = fread(b, 1, 63, f); b[r] = 0; fclose(f);
    printf("%s: [%s]\n", what, b);
}

int main(void)
{
    FILE *f = fopen("turn.dat", "w"); fputs("abc", f); fclose(f);

    f = fopen("turn.dat", "r+");
    int n = 0; while (fgetc(f) != EOF) n++;
    fputs("XYZ", f);
    fclose(f);
    printf("read %d; ", n); show("fputs after the end");

    f = fopen("turn.dat", "r+");
    char b[16]; n = (int)fread(b, 1, 16, f);
    fputc('!', f); fputc('?', f);
    fclose(f);
    printf("read %d; ", n); show("fputc after the end");

    f = fopen("turn.dat", "r+");
    while (fgetc(f) != EOF) ;
    fwrite("#", 1, 1, f);                       /* one byte: fwrite's own entry */
    fwrite("0123456789", 1, 10, f);
    long t = ftell(f);
    fclose(f);
    printf("at %ld; ", t); show("fwrite after the end");

    /* SEEK_CUR: one character read, a hundred read ahead */
    f = fopen("turn.dat", "w"); for (int i = 0; i < 100; i++) fputc('a' + i % 26, f); fclose(f);
    f = fopen("turn.dat", "r");
    int c1 = fgetc(f);
    int r = fseek(f, 0, SEEK_CUR); long t0 = ftell(f); int c2 = fgetc(f);
    r |= fseek(f, 10, SEEK_CUR); long t1 = ftell(f); int c3 = fgetc(f);
    r |= fseek(f, -5, SEEK_CUR); long t2 = ftell(f); int c4 = fgetc(f);
    printf("%c | %ld %c | %ld %c | %ld %c | rc %d\n", c1, t0, c2, t1, c3, t2, c4, r);
    /* a character put back is one not yet read */
    fseek(f, 20, SEEK_SET); int c5 = fgetc(f); ungetc(c5, f); long t3 = ftell(f);
    fseek(f, 0, SEEK_CUR); long t4 = ftell(f); int c6 = fgetc(f);
    printf("%c | %ld | %ld %c\n", c5, t3, t4, c6);
    fclose(f);

    /* SEEK_CUR while writing: the bytes not yet flushed count */
    f = fopen("turn.dat", "w");
    fputs("0123456789", f);
    fseek(f, -4, SEEK_CUR); fputs("ab", f); long t5 = ftell(f);
    fseek(f, 1, SEEK_CUR); fputc('Z', f);
    fclose(f);
    printf("at %ld; ", t5); show("SEEK_CUR while writing");

    /* read, seek from here, write in place, read on */
    f = fopen("turn.dat", "r+");
    fread(b, 1, 3, f);
    fseek(f, 0, SEEK_CUR); fputs("--", f); fseek(f, 0, SEEK_CUR);
    int c7 = fgetc(f);
    fclose(f);
    printf("next %c; ", c7); show("in place");

    remove("turn.dat");
    return 0;
}
