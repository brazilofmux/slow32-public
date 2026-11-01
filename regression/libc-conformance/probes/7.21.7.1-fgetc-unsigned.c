/* 7.21.7.1 fgetc: returns the next byte as an unsigned char converted to int, EOF only at end of file or error */
#include <stdio.h>

int main(void) {
    FILE *f = fopen("bytes.bin", "wb+");
    if (!f) { printf("fopen failed\n"); return 1; }
    printf("%d %d %d\n", fputc(0xff, f), fputc(0x180, f), fputc(0, f));
    rewind(f);
    int a = fgetc(f), b = getc(f), c = fgetc(f), d = fgetc(f);
    printf("%d %d %d %d %d\n", a, b, c, d == EOF, feof(f) != 0);
    fclose(f);
    remove("bytes.bin");
    return 0;
}
