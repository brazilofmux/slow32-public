/* cc-min main: driver for the minimal C compiler.
 * Uses io_* wrappers from pass1 to avoid cc.fth long-call bug.
 * stderr declared as global — libc's __stdio_init() initialises it. */

int stderr;

int ccmin_load_source(const char *path);
int pass1_parse_to_ir(void);
int pass2_validate_ir(void);
int pass3_emit_from_ir(const char *out_path);

int main(int argc, char **argv) {
    if (argc != 3) {
        io_fputs("usage: ", stderr);
        io_fputs(argv[0], stderr);
        io_fputs(" <input.c> <output.s>\n", stderr);
        return 1;
    }
    if (!ccmin_load_source(argv[1])) {
        io_fputs("error: unable to read ", stderr);
        io_fputs(argv[1], stderr);
        io_fputc(10, stderr);
        return 1;
    }
    if (!pass1_parse_to_ir()) {
        io_fputs("error: unsupported source shape\n", stderr);
        return 1;
    }
    if (!pass2_validate_ir()) {
        io_fputs("error: return immediate out of range\n", stderr);
        return 1;
    }
    if (!pass3_emit_from_ir(argv[2])) {
        io_fputs("error: unable to write ", stderr);
        io_fputs(argv[2], stderr);
        io_fputc(10, stderr);
        return 1;
    }
    return 0;
}
