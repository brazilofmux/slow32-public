#include <stdio.h>

int ccmin_load_source(const char *path);
int pass1_parse_to_ir(void);
int pass2_validate_ir(void);
int pass3_emit_from_ir(const char *out_path);

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "usage: %s <input.c> <output.s>\n", argv[0]);
        return 1;
    }
    if (!ccmin_load_source(argv[1])) {
        fprintf(stderr, "error: unable to read %s\n", argv[1]);
        return 1;
    }
    if (!pass1_parse_to_ir()) {
        fprintf(stderr, "error: unsupported source shape\n");
        return 1;
    }
    if (!pass2_validate_ir()) {
        fprintf(stderr, "error: return immediate out of range\n");
        return 1;
    }
    if (!pass3_emit_from_ir(argv[2])) {
        fprintf(stderr, "error: unable to write %s\n", argv[2]);
        return 1;
    }
    return 0;
}
