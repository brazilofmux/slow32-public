#include <stdio.h>
#include <stdint.h>

static void w32(FILE *f, uint32_t v) {
    fputc((int)(v & 255), f);
    fputc((int)((v >> 8) & 255), f);
    fputc((int)((v >> 16) & 255), f);
    fputc((int)((v >> 24) & 255), f);
}

int main(int argc, char **argv) {
    FILE *out;
    if (argc != 3) return 1;
    out = fopen(argv[2], "wb");
    if (!out) return 2;
    w32(out, 0x5333324Fu);
    fclose(out);
    return 0;
}
