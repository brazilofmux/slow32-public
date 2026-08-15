/* Guest-side test for fmemopen / open_memstream / getline (memstream.c). */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int failures;

static void check(int cond, const char *msg) {
    if (!cond) {
        printf("FAIL: %s\n", msg);
        failures++;
    }
}

int main(void) {
    /* open_memstream: build, flush mid-stream, close, inspect */
    {
        char *buf = NULL;
        size_t len = 0;
        FILE *m = open_memstream(&buf, &len);
        check(m != NULL, "open_memstream returns a stream");
        fputs("hello", m);
        fflush(m);
        check(len == 5 && buf && memcmp(buf, "hello", 5) == 0,
              "flush syncs buffer and length");
        fprintf(m, " %s #%d", "world", 7);
        /* force growth past the initial 64-byte capacity */
        {
            int i;
            for (i = 0; i < 40; i++) fputs("xyz", m);
        }
        fclose(m);
        check(len == 5 + 9 + 120, "final length after growth");
        check(buf[len] == '\0', "buffer NUL-terminated");
        check(memcmp(buf, "hello world #7xyz", 17) == 0, "content in order");
        free(buf);
    }

    /* fmemopen "r": fgetc/fread/EOF/seek/tell */
    {
        const char *text = "alpha\nbeta\n";
        FILE *m = fmemopen((void *)text, strlen(text), "r");
        char word[16];
        check(m != NULL, "fmemopen r returns a stream");
        check(fgetc(m) == 'a', "fgetc first byte");
        check(fread(word, 1, 4, m) == 4 && memcmp(word, "lpha", 4) == 0,
              "fread continues from position");
        check(ftell(m) == 5, "ftell after reads");
        check(fseek(m, 0, SEEK_END) == 0 && ftell(m) == 11, "seek to end");
        check(fgetc(m) == EOF && feof(m), "EOF at end");
        check(fseek(m, 6, SEEK_SET) == 0, "seek back");
        check(!feof(m), "seek clears EOF");
        check(fgets(word, sizeof word, m) && strcmp(word, "beta\n") == 0,
              "fgets after seek");
        fclose(m);
    }

    /* getline over a memstream, including the unterminated last line */
    {
        const char *text = "one\ntwo\nthree";
        FILE *m = fmemopen((void *)text, strlen(text), "r");
        char *line = NULL;
        size_t cap = 0;
        check(getline(&line, &cap, m) == 4 && strcmp(line, "one\n") == 0,
              "getline line 1");
        check(getline(&line, &cap, m) == 4 && strcmp(line, "two\n") == 0,
              "getline line 2");
        check(getline(&line, &cap, m) == 5 && strcmp(line, "three") == 0,
              "getline unterminated last line");
        check(getline(&line, &cap, m) == -1, "getline EOF");
        free(line);
        fclose(m);
    }

    /* getline growth: a line longer than the initial 128-byte buffer */
    {
        char big[300];
        FILE *m;
        char *line = NULL;
        size_t cap = 0;
        memset(big, 'q', sizeof(big));
        big[298] = '\n';
        big[299] = '\0';
        m = fmemopen(big, 299, "r");
        check(getline(&line, &cap, m) == 299, "getline long line length");
        check(line[0] == 'q' && line[297] == 'q' && line[298] == '\n' &&
              line[299] == '\0', "getline long line content");
        free(line);
        fclose(m);
    }

    /* getline on a real MMIO file: the same code path mdfix read_all uses */
    {
        const char *path = "/tmp/slow32-memstream-getline.txt";
        FILE *f = fopen(path, "w");
        char *line = NULL;
        size_t cap = 0;
        check(f != NULL, "fixture file created");
        fputs("first\nsecond\n", f);
        fclose(f);
        f = fopen(path, "r");
        check(f != NULL, "fixture file reopens");
        check(getline(&line, &cap, f) == 6 && strcmp(line, "first\n") == 0,
              "getline from MMIO file");
        check(getline(&line, &cap, f) == 7 && strcmp(line, "second\n") == 0,
              "getline second line");
        check(getline(&line, &cap, f) == -1, "getline file EOF");
        free(line);
        fclose(f);
        remove(path);
    }

    /* The mdfix render_converged shape: write a memstream, re-read it via
     * fmemopen, write a second memstream, compare. */
    {
        char *b1 = NULL, *b2 = NULL;
        size_t l1 = 0, l2 = 0;
        FILE *out1 = open_memstream(&b1, &l1);
        FILE *in;
        FILE *out2;
        int c;
        fputs("- item\n", out1);
        fclose(out1);
        in = fmemopen(b1, l1, "r");
        out2 = open_memstream(&b2, &l2);
        while ((c = fgetc(in)) != EOF) fputc(c, out2);
        fclose(in);
        fclose(out2);
        check(l1 == l2 && memcmp(b1, b2, l1) == 0,
              "round-trip through fmemopen converges");
        free(b1);
        free(b2);
    }

    /* fmemopen fixed "w": fills, then errors, never grows */
    {
        char fixed[8];
        FILE *m = fmemopen(fixed, sizeof(fixed), "w");
        size_t wrote = fwrite("0123456789AB", 1, 12, m);
        check(wrote == 8, "fixed buffer takes exactly its size");
        check(ferror(m), "overflow sets error");
        fclose(m);
        check(memcmp(fixed, "01234567", 8) == 0, "fixed buffer content");
    }

    if (failures == 0) {
        printf("memstream: all tests passed\n");
        return 0;
    }
    printf("memstream: %d FAILURES\n", failures);
    return 1;
}
