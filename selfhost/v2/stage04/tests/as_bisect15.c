#include <stdio.h>
#include <stdint.h>
#include <s32_formats.h>

static FILE *g_out = 0;
static uint8_t g_text[1024];
static uint8_t g_data[64];
static char g_str[256];

static void w16(uint16_t v) { fputc((int)(v & 255), g_out); fputc((int)((v >> 8) & 255), g_out); }
static void w32(uint32_t v) { fputc((int)(v & 255), g_out); fputc((int)((v >> 8) & 255), g_out); fputc((int)((v >> 16) & 255), g_out); fputc((int)((v >> 24) & 255), g_out); }

int main(int argc, char **argv) {
    uint32_t i;
    if (argc != 3) return 1;
    g_out = fopen(argv[2], "wb");
    if (!g_out) return 2;

    for (i = 0; i < 16; i++) g_text[i] = (uint8_t)i;
    for (i = 0; i < 4; i++) g_data[i] = (uint8_t)(0xA0 + i);

    g_str[0] = 0;
    g_str[1] = '.'; g_str[2] = 't'; g_str[3] = 'e'; g_str[4] = 'x'; g_str[5] = 't'; g_str[6] = 0;
    g_str[7] = '.'; g_str[8] = 'd'; g_str[9] = 'a'; g_str[10] = 't'; g_str[11] = 'a'; g_str[12] = 0;

    w32(S32O_MAGIC); w16(1); fputc(S32_ENDIAN_LITTLE, g_out); fputc(S32_MACHINE_SLOW32, g_out);
    w32(0); w32(2); w32(40); w32(0); w32(104); w32(104); w32(13); w32(0);

    w32(1); w32(S32_SEC_CODE); w32(S32_SEC_FLAG_EXEC | S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC);
    w32(16); w32(120); w32(4); w32(0); w32(0);

    w32(7); w32(S32_SEC_DATA); w32(S32_SEC_FLAG_WRITE | S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC);
    w32(4); w32(136); w32(4); w32(0); w32(0);

    fwrite(g_str, 1, 13, g_out);
    while (((uint32_t)ftell(g_out) & 3u) != 0) fputc(0, g_out);
    fwrite(g_text, 1, 16, g_out);
    fwrite(g_data, 1, 4, g_out);

    fclose(g_out);
    g_out = 0;
    return 0;
}
