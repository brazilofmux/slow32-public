#include <stdio.h>
#include <stdint.h>

#define A 1
#define B 4
#define C 8

static void w32(FILE *f, uint32_t v) {
    fputc((int)(v & 255), f);
    fputc((int)((v >> 8) & 255), f);
    fputc((int)((v >> 16) & 255), f);
    fputc((int)((v >> 24) & 255), f);
}

int main(int argc, char **argv) {
    FILE *out;
    uint32_t x;
    uint32_t y;
    if (argc != 3) return 1;
    out = fopen(argv[2], "wb");
    if (!out) return 2;
    x = A | B | C;
    y = A | C;
    w32(out, x);
    w32(out, y);
    fclose(out);
    return 0;
}
