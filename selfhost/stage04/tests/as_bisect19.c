#include <stdio.h>

static void w32i(FILE *f, int v) {
    fputc(v & 255, f);
    fputc((v >> 8) & 255, f);
    fputc((v >> 16) & 255, f);
    fputc((v >> 24) & 255, f);
}

int main(int argc, char **argv) {
    FILE *out;
    if (argc != 3) return 1;
    out = fopen(argv[2], "wb");
    if (!out) return 2;
    w32i(out, 0x5333324F);
    w32i(out, 0x11223344);
    fclose(out);
    return 0;
}
