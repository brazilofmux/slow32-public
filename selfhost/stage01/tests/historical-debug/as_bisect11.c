#include <stdio.h>
#include <stdint.h>
#include <s32_formats.h>

int main(int argc, char **argv) {
    FILE *out;
    if (argc != 3) return 1;
    out = fopen(argv[2], "wb");
    if (!out) return 2;
    fputc((int)(S32O_MAGIC & 255), out);
    fclose(out);
    return 0;
}
