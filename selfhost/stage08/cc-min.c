/* cc-min main: driver for the minimal C compiler.
 * stderr declared as global — libc's __stdio_init() initialises it. */

int stderr;

int ccmin_load_source(const char *path);
int pass1_parse_to_ir(void);
int pass2_validate_ir(void);
int pass3_emit_from_ir(const char *out_path);

int main(int argc, char **argv) {
    if (argc != 3) {
        fputs("usage: ", stderr);
        fputs(argv[0], stderr);
        fputs(" <input.c> <output.s>\n", stderr);
        return 1;
    }
    if (!ccmin_load_source(argv[1])) {
        fputs("error: unable to read ", stderr);
        fputs(argv[1], stderr);
        fputc(10, stderr);
        return 1;
    }
    if (!pass1_parse_to_ir()) {
        fputs("error: unsupported source shape\n", stderr);
        return 1;
    }
    if (!pass2_validate_ir()) {
        fputs("error: return immediate out of range\n", stderr);
        return 1;
    }
    if (!pass3_emit_from_ir(argv[2])) {
        fputs("error: unable to write ", stderr);
        fputs(argv[2], stderr);
        fputc(10, stderr);
        return 1;
    }
    return 0;
}
