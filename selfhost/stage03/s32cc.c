#include "s32cc_combined.h"

int main(int argc, char **argv) {
    int rc;
    int f;
    int i;
    if (argc < 3) {
        fputs("usage: s32cc input.c output.s\n", stderr);
        return 1;
    }
    rc = s32cc_compile(argv[1]);
    if (rc != 0) return rc;
    f = fopen(argv[2], "wb");
    if (!f) {
        fputs("s32cc: cannot open output file\n", stderr);
        return 1;
    }
    i = 0;
    while (i < p_olen) {
        fputc(p_out[i], f);
        i = i + 1;
    }
    fclose(f);
    return 0;
}
