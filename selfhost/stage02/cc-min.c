/* cc-min main: driver for the minimal C compiler.
 * Uses fdputs/fdputc for error output (fd 2 = stderr). */

int ccmin_load_source(const char *path);
int pass1_parse_to_ir(void);
int pass2_validate_ir(void);
int pass3_emit_from_ir(const char *out_path);

int main(int argc, char **argv) {
    if (argc != 3) {
        fdputs("usage: ", 2);
        fdputs(argv[0], 2);
        fdputs(" <input.c> <output.s>\n", 2);
        return 1;
    }
    if (!ccmin_load_source(argv[1])) {
        fdputs("error: unable to read ", 2);
        fdputs(argv[1], 2);
        fdputc(10, 2);
        return 1;
    }
    if (!pass1_parse_to_ir()) {
        fdputs("error: unsupported source shape\n", 2);
        return 1;
    }
    if (!pass2_validate_ir()) {
        fdputs("error: return immediate out of range\n", 2);
        return 1;
    }
    if (!pass3_emit_from_ir(argv[2])) {
        fdputs("error: unable to write ", 2);
        fdputs(argv[2], 2);
        fdputc(10, 2);
        return 1;
    }
    return 0;
}
