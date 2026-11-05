/* HOSTLEG
 * open_memstream: what has been written is there after fflush and fclose,
 * NUL-terminated, past the first block and across the stream's own buffer. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
    char *body = NULL;
    size_t len = 0;
    FILE *m = open_memstream(&body, &len);
    int i, sum = 0;
    if (!m) { printf("no stream\n"); return 1; }
    fprintf(m, "first %d", 41);
    fflush(m);
    printf("[%s] %d\n", body, (int)len);
    fputs(", then", m);
    fputc('!', m);
    fflush(m);
    printf("[%s] %d\n", body, (int)len);
    for (i = 0; i < 3000; i++) fprintf(m, "%04d,", i);
    fwrite("tail", 1, 4, m);
    if (fclose(m) != 0) printf("close failed\n");
    printf("%d %d\n", (int)len, (int)strlen(body));
    printf("%.20s ... %s\n", body, body + len - 10);
    for (i = 0; i < (int)len; i++) sum = (sum * 31 + (unsigned char)body[i]) & 0xFFFFFF;
    printf("%d\n", sum);
    free(body);

    /* two at once, and a file between them */
    {
        char *a = NULL, *b = NULL;
        size_t al = 0, bl = 0;
        FILE *x = open_memstream(&a, &al);
        FILE *y = open_memstream(&b, &bl);
        fputs("alpha", x);
        fputs("beta", y);
        fputs("gamma", x);
        fclose(y);
        fclose(x);
        printf("%s %d %s %d\n", a, (int)al, b, (int)bl);
        free(a);
        free(b);
    }

    /* an empty one is an empty string */
    {
        char *e = NULL;
        size_t el = 7;
        FILE *z = open_memstream(&e, &el);
        fclose(z);
        printf("[%s] %d\n", e, (int)el);
        free(e);
    }
    printf("done\n");
    return 0;
}
