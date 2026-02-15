#include <stdio.h>
#include <stdint.h>

/* Passes: array count fits current type encoding. */
static uint8_t g_buf[1023];

int main(int argc, char **argv) {
    FILE *out;
    if (argc != 3) return 1;
    out = fopen(argv[2], "wb");
    if (!out) return 2;
    g_buf[0] = 1;
    g_buf[1] = 2;
    fwrite(g_buf, 1, 2, out);
    fclose(out);
    return 0;
}
