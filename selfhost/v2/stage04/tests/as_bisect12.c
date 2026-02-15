#include <stdio.h>
#include <stdint.h>

int main(int argc, char **argv) {
    FILE *out;
    uint32_t v;
    if (argc != 3) return 1;
    out = fopen(argv[2], "wb");
    if (!out) return 2;
    v = 0x5333324F;
    fputc((int)(v & 255), out);
    fputc((int)((v >> 8) & 255), out);
    fputc((int)((v >> 16) & 255), out);
    fputc((int)((v >> 24) & 255), out);
    fclose(out);
    return 0;
}
