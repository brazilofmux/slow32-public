#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <s32_formats.h>

static uint8_t g_text[1024];
static uint8_t g_data[64];
static char g_str[256];

static void put_u16(FILE *f, uint16_t v) {
    fputc((int)(v & 255), f);
    fputc((int)((v >> 8) & 255), f);
}

int main(int argc, char **argv) {
    FILE *out;
    uint32_t i;
    if (argc != 3) return 1;
    out = fopen(argv[2], "wb");
    if (!out) return 2;

    for (i = 0; i < 16; i++) g_text[i] = (uint8_t)i;
    for (i = 0; i < 4; i++) g_data[i] = (uint8_t)(0xA0 + i);
    g_str[0] = 0; strcpy(g_str + 1, ".text"); strcpy(g_str + 7, ".data");

    /* inline 32-bit writes */
    fputc((int)(S32O_MAGIC & 255), out); fputc((int)((S32O_MAGIC >> 8) & 255), out); fputc((int)((S32O_MAGIC >> 16) & 255), out); fputc((int)((S32O_MAGIC >> 24) & 255), out);
    put_u16(out, 1); fputc(S32_ENDIAN_LITTLE, out); fputc(S32_MACHINE_SLOW32, out);

    /* header fields */
    {
        uint32_t h[8];
        int k;
        h[0]=0; h[1]=2; h[2]=40; h[3]=0; h[4]=104; h[5]=104; h[6]=13; h[7]=0;
        for (k = 0; k < 8; k++) {
            uint32_t v = h[k];
            fputc((int)(v & 255), out); fputc((int)((v >> 8) & 255), out); fputc((int)((v >> 16) & 255), out); fputc((int)((v >> 24) & 255), out);
        }
    }

    /* section 1 */
    {
        uint32_t s[8];
        int k;
        s[0]=1; s[1]=S32_SEC_CODE; s[2]=S32_SEC_FLAG_EXEC | S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC; s[3]=16; s[4]=120; s[5]=4; s[6]=0; s[7]=0;
        for (k = 0; k < 8; k++) {
            uint32_t v = s[k];
            fputc((int)(v & 255), out); fputc((int)((v >> 8) & 255), out); fputc((int)((v >> 16) & 255), out); fputc((int)((v >> 24) & 255), out);
        }
    }

    /* section 2 */
    {
        uint32_t s[8];
        int k;
        s[0]=7; s[1]=S32_SEC_DATA; s[2]=S32_SEC_FLAG_WRITE | S32_SEC_FLAG_READ | S32_SEC_FLAG_ALLOC; s[3]=4; s[4]=136; s[5]=4; s[6]=0; s[7]=0;
        for (k = 0; k < 8; k++) {
            uint32_t v = s[k];
            fputc((int)(v & 255), out); fputc((int)((v >> 8) & 255), out); fputc((int)((v >> 16) & 255), out); fputc((int)((v >> 24) & 255), out);
        }
    }

    fwrite(g_str, 1, 13, out);
    while (((uint32_t)ftell(out) & 3u) != 0) fputc(0, out);
    fwrite(g_text, 1, 16, out);
    fwrite(g_data, 1, 4, out);

    fclose(out);
    return 0;
}
