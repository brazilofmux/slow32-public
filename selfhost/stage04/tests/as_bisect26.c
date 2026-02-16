#include <stdio.h>
#include <stdint.h>

/* Fails today: 1024 overflows current array-count encoding (10 bits). */
static uint8_t g_buf[1024];

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
