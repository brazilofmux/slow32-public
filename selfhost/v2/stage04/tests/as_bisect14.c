#include <stdio.h>
#include <stdint.h>

static uint8_t g_buf[16];

int main(int argc, char **argv) {
    FILE *out;
    int i;
    if (argc != 3) return 1;
    out = fopen(argv[2], "wb");
    if (!out) return 2;
    for (i = 0; i < 16; i++) g_buf[i] = (uint8_t)i;
    fwrite(g_buf, 1, 16, out);
    while (((uint32_t)ftell(out) & 3u) != 0) {
        fputc(0, out);
    }
    fclose(out);
    return 0;
}
