/* HOSTLEG -- the stream functions the self-hosted library did not have
 * or only pretended to: freopen, fdopen, getline, tmpfile, fgetpos and
 * fsetpos, setbuf, the v*printf family, what printf and the puts family
 * return. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <fcntl.h>
#include <unistd.h>

static int vf(FILE *f, const char *fmt, ...) {
    va_list ap;
    int n;
    va_start(ap, fmt);
    n = vfprintf(f, fmt, ap);
    va_end(ap);
    return n;
}

static int vs(char *b, const char *fmt, ...) {
    va_list ap;
    int n;
    va_start(ap, fmt);
    n = vsprintf(b, fmt, ap);
    va_end(ap);
    return n;
}

static int vsn(char *b, unsigned int size, const char *fmt, ...) {
    va_list ap;
    int n;
    va_start(ap, fmt);
    n = vsnprintf(b, size, fmt, ap);
    va_end(ap);
    return n;
}

static int vp(const char *fmt, ...) {
    va_list ap;
    int n;
    va_start(ap, fmt);
    n = vprintf(fmt, ap);
    va_end(ap);
    return n;
}

int main(void) {
    FILE *f, *g;
    char buf[64];
    char *line;
    size_t cap;
    long len;
    fpos_t pos;
    int n, fd, c, i;

    n = printf("printf returns its count\n");
    printf("%d\n", n);
    n = vp("vprintf %d %s %c\n", 42, "str", 'x');
    printf("%d\n", n);
    n = vs(buf, "%05d|%-4s|%x", 42, "ab", 255);
    printf("vsprintf %d [%s]\n", n, buf);
    memset(buf, '#', sizeof buf);
    n = vsn(buf, 8, "%s", "truncated here");
    printf("vsnprintf %d [%s]\n", n, buf);
    n = vsn(buf, 0, "%d", 12345);
    printf("vsnprintf into nothing %d\n", n);
    n = snprintf(buf, 4, "%d", 123456);
    printf("snprintf %d [%s]\n", n, buf);
    n = puts("puts adds the newline");
    printf("puts %s\n", n >= 0 ? "nonnegative" : "NEGATIVE");
    n = fputs("fputs does not\n", stdout);
    printf("fputs %s\n", n >= 0 ? "nonnegative" : "NEGATIVE");
    printf("putchar %d fputc %d putc %d\n", putchar('A'), fputc('B', stdout), putc('\n', stdout));

    /* getline: lines of every length, the last without a newline */
    f = fopen("more_lines.txt", "w");
    n = vf(f, "short\n\n%s\nlast, unterminated", "a line long enough that the first buffer getline was given cannot hold it, so it must grow, and then grow again: 0123456789 0123456789 0123456789 0123456789 0123456789 0123456789");
    printf("vfprintf %d\n", n);
    fclose(f);
    f = fopen("more_lines.txt", "r");
    line = 0; cap = 0;
    while ((len = (long)getline(&line, &cap, f)) != -1)
        printf("getline %ld (capacity %s) ends %d [%.20s]\n", len, cap > (size_t)len ? "holds it" : "TOO SMALL",
               line[len - 1] == '\n', line);
    printf("getline at the end: %ld, feof %d\n", len, feof(f) != 0);
    free(line);

    /* getdelim: the same, to a delimiter of the caller's */
    rewind(f);
    line = 0; cap = 0;
    while ((len = (long)getdelim(&line, &cap, ',', f)) != -1)
        printf("getdelim %ld %s\n", len, line[len - 1] == ',' ? "to a comma" : "to the end");
    free(line);

    /* fgetpos / fsetpos */
    rewind(f);
    fgets(buf, sizeof buf, f);
    fgetpos(f, &pos);
    fgets(buf, sizeof buf, f);
    c = fgetc(f);
    printf("after two lines: %c\n", c);
    printf("fsetpos %d\n", fsetpos(f, &pos));
    fgets(buf, sizeof buf, f);
    printf("the second line again: %d bytes\n", (int)strlen(buf));
    c = fgetc(f);
    printf("and then: %c\n", c);

    /* freopen: the same FILE, another file */
    g = freopen("more_other.txt", "w", f);
    printf("freopen returns the stream: %d\n", g == f);
    fputs("written through the reopened stream\n", f);
    fclose(f);
    f = fopen("more_other.txt", "r");
    fgets(buf, sizeof buf, f);
    printf("it holds: %s", buf);
    g = freopen("more_lines.txt", "r", f);
    fgets(buf, sizeof buf, g);
    printf("reopened for reading: %s", buf);
    printf("freopen of a file that is not there: %s\n", freopen("more_no_such_file.txt", "r", g) ? "A STREAM" : "null");

    /* ...and when the new file's descriptor is not the old one's number
     * (a lower one has come free), the stream follows the file */
    {
        FILE *lo = fopen("more_lo.txt", "w");
        FILE *hi = fopen("more_hi.txt", "w");
        fputs("low\n", lo);
        fclose(lo);                                  /* its number is free again */
        fputs("high, before\n", hi);
        g = freopen("more_moved.txt", "w", hi);      /* closes hi's, opens on the freed one */
        fputs("written after the move\n", g);
        fclose(g);
        f = fopen("more_moved.txt", "r");
        fgets(buf, sizeof buf, f);
        printf("after a move: %s", buf);
        fclose(f);
        f = fopen("more_hi.txt", "r");
        fgets(buf, sizeof buf, f);
        printf("the file it left: %s", buf);
        printf("and nothing more there: %d\n", fgetc(f) == EOF);
        fclose(f);
        printf("remove %d %d %d\n", remove("more_lo.txt"), remove("more_hi.txt"), remove("more_moved.txt"));
    }

    /* fdopen: a stream on a descriptor already open */
    fd = open("more_fd.txt", O_WRONLY | O_CREAT | O_TRUNC, 0644);
    f = fdopen(fd, "w");
    printf("fdopen %s, fileno is the descriptor: %d\n", f ? "a stream" : "NULL", f && fileno(f) == fd);
    fprintf(f, "through fdopen %d\n", 77);
    fclose(f);
    fd = open("more_fd.txt", O_RDONLY);
    f = fdopen(fd, "r");
    fgets(buf, sizeof buf, f);
    printf("read back: %s", buf);
    printf("then EOF: %d\n", fgetc(f) == EOF);
    fclose(f);
    printf("the descriptor is closed with the stream: %d\n", close(fd) != 0);

    /* tmpfile: a file with no name, for reading and writing */
    f = tmpfile();
    if (!f) {
        printf("tmpfile: NULL\n");
    } else {
        for (i = 0; i < 2000; i++) fprintf(f, "%d\n", i * 7);
        rewind(f);
        n = 0;
        while (fgets(buf, sizeof buf, f)) if (atoi(buf) == n * 7) n++;
        printf("tmpfile: %d lines read back\n", n);
        fseek(f, 0, SEEK_END);
        printf("tmpfile: size %ld\n", ftell(f));
        fclose(f);
        g = tmpfile();
        printf("tmpfile: a second one is empty: %d\n", g && fgetc(g) == EOF);
        if (g) fclose(g);
    }

    /* setbuf: a buffer of the caller's, or none */
    f = fopen("more_setbuf.txt", "w");
    {
        static char mine[BUFSIZ];
        setbuf(f, mine);
        fputs("buffered in the caller's array\n", f);
        fclose(f);
    }
    f = fopen("more_setbuf.txt", "a");
    setbuf(f, 0);
    fputs("unbuffered\n", f);
    g = fopen("more_setbuf.txt", "r");     /* unbuffered: already in the file */
    n = 0;
    while (fgets(buf, sizeof buf, g)) n++;
    printf("setbuf: %d lines in the file before the close\n", n);
    fclose(g);
    fclose(f);

    /* freopen of stdout: what printf writes from here on is in a file */
    fflush(stdout);
    if (freopen("more_stdout.txt", "w", stdout) == 0) {
        fprintf(stderr, "freopen(stdout) FAILED\n");
    } else {
        printf("printed after stdout was reopened\n");
        puts("and a second line");
        fclose(stdout);
        f = fopen("more_stdout.txt", "r");
        while (fgets(buf, sizeof buf, f)) fprintf(stderr, "in the file: %s", buf);
        fclose(f);
        fprintf(stderr, "remove %d\n", remove("more_stdout.txt"));
    }
    fprintf(stderr, "remove %d %d %d %d, and again %s\n", remove("more_lines.txt"), remove("more_other.txt"),
           remove("more_fd.txt"), remove("more_setbuf.txt"), remove("more_lines.txt") != 0 ? "fails" : "SUCCEEDS");
    return 0;
}
