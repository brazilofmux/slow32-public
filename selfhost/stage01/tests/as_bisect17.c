#include <stdio.h>
#include <stdint.h>
#include <s32_formats.h>

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
    w32(out, S32O_MAGIC);
    w32(out, S32_SEC_CODE);
    w32(out, S32_SEC_DATA);
    w32(out, S32_SEC_FLAG_EXEC | S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC);
    w32(out, S32_SEC_FLAG_WRITE | S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC);
    w32(out, 40);
    w32(out, 104);
    w32(out, 136);
    w32(out, 13);
    w32(out, 0x11223344u);
    fclose(out);
    return 0;
}
