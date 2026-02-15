#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <s32_formats.h>

static uint8_t g_text[1024];
static uint8_t g_data[64];
static char g_str[256];

static void w16(FILE *f, uint16_t v) { fputc((int)(v & 255), f); fputc((int)((v >> 8) & 255), f); }
static void w32(FILE *f, uint32_t v) { fputc((int)(v & 255), f); fputc((int)((v >> 8) & 255), f); fputc((int)((v >> 16) & 255), f); fputc((int)((v >> 24) & 255), f); }

int main(int argc, char **argv) {
    FILE *out;
    uint32_t i;
    if (argc != 3) return 1;
    out = fopen(argv[2], "wb");
    if (!out) return 2;

    for (i = 0; i < 16; i++) g_text[i] = (uint8_t)i;
    for (i = 0; i < 4; i++) g_data[i] = (uint8_t)(0xA0 + i);
    g_str[0] = 0; strcpy(g_str + 1, ".text"); strcpy(g_str + 7, ".data");

    w32(out, S32O_MAGIC); w16(out, 1); fputc(S32_ENDIAN_LITTLE, out); fputc(S32_MACHINE_SLOW32, out);
    w32(out, 0); w32(out, 2); w32(out, 40); w32(out, 0); w32(out, 104); w32(out, 104); w32(out, 13); w32(out, 0);

    w32(out, 1); w32(out, S32_SEC_CODE); w32(out, S32_SEC_FLAG_EXEC | S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC);
    w32(out, 16); w32(out, 120); w32(out, 4); w32(out, 0); w32(out, 0);

    w32(out, 7); w32(out, S32_SEC_DATA); w32(out, S32_SEC_FLAG_WRITE | S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC);
    w32(out, 4); w32(out, 136); w32(out, 4); w32(out, 0); w32(out, 0);

    fwrite(g_str, 1, 13, out);
    while (((uint32_t)ftell(out) & 3u) != 0) fputc(0, out);
    fwrite(g_text, 1, 16, out);
    fwrite(g_data, 1, 4, out);

    fclose(out);
    return 0;
}
