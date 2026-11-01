/* 7.21.7.10 ungetc: clears the end-of-file indicator, decrements the binary-stream position (a second pushback may fail, but if it succeeds it decrements again), and is discarded by fseek */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("ungpos.bin", "wb+");
    if (!f) { printf("fopen failed\n"); return 1; }
    fputs("hello", f);
    rewind(f);
    fgetc(f); fgetc(f); fgetc(f);
    printf("%ld", ftell(f));
    ungetc('l', f);
    printf(" %ld", ftell(f));
    int r = ungetc('X', f);
    long p = ftell(f);
    printf(" %d\n", (r == EOF && p == 2) || (r == 'X' && p == 1));
    fseek(f, 0, SEEK_END);
    printf("%d %d", fgetc(f) == EOF, feof(f) != 0);
    ungetc('!', f);
    printf(" %d %c %d\n", feof(f) != 0, fgetc(f), fgetc(f) == EOF);
    fseek(f, 1, SEEK_SET);
    ungetc('Q', f);
    fseek(f, 1, SEEK_SET);
    printf("%c\n", fgetc(f));
    fclose(f);
    remove("ungpos.bin");
    return 0;
}
