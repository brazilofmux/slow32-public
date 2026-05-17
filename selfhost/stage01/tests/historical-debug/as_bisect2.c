#include <stdio.h>

int main(int argc, char **argv) {
    FILE *in;
    FILE *out;
    if (argc != 3) return 1;
    in = fopen(argv[1], "rb");
    if (!in) return 2;
    out = fopen(argv[2], "wb");
    if (!out) { fclose(in); return 3; }
    fclose(out);
    fclose(in);
    return 0;
}
