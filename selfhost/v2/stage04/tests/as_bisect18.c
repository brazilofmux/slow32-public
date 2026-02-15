#include <stdio.h>

static void w8(FILE *f, int v) {
    fputc(v & 255, f);
}

int main(int argc, char **argv) {
    FILE *out;
    if (argc != 3) return 1;
    out = fopen(argv[2], "wb");
    if (!out) return 2;
    w8(out, 0x41);
    w8(out, 0x42);
    w8(out, 0x43);
    w8(out, 0x44);
    fclose(out);
    return 0;
}
