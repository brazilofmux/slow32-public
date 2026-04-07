#include "s32cc_combined.h"

int main(int argc, char **argv) {
    int rc;
    int f;
    int i;
    if (argc < 3) {
        fdputs("usage: s32cc input.c output.s\n", 2);
        return 1;
    }
    rc = s32cc_compile(argv[1]);
    if (rc != 0) return rc;
    f = fdopen_path(argv[2], "wb");
    if (!f) {
        fdputs("s32cc: cannot open output file\n", 2);
        return 1;
    }
    i = 0;
    while (i < p_olen) {
        fdputc(p_out[i], f);
        i = i + 1;
    }
    fdclose(f);
    return 0;
}
