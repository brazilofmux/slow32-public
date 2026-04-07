/* cc-min pass3: write assembly output to file. */

int ccmin_get_output_len(void);
char *ccmin_get_output_buf(void);

int pass3_emit_from_ir(const char *out_path) {
    int f;
    char *buf;
    int len;
    int i;

    f = fdopen_path(out_path, "wb");
    if (!f) return 0;
    buf = ccmin_get_output_buf();
    len = ccmin_get_output_len();
    i = 0;
    while (i < len) {
        fdputc(buf[i], f);
        i = i + 1;
    }
    if (fdclose(f) != 0) return 0;
    return 1;
}
