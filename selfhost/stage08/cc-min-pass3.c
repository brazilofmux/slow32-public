/* cc-min pass3: write assembly output to file.
 * Uses io_* wrappers from pass1 to avoid cc.fth long-call bug. */

int ccmin_get_output_len(void);
char *ccmin_get_output_buf(void);

int pass3_emit_from_ir(const char *out_path) {
    int f;
    char *buf;
    int len;
    int i;

    f = io_fopen(out_path, "wb");
    if (!f) return 0;
    buf = ccmin_get_output_buf();
    len = ccmin_get_output_len();
    i = 0;
    while (i < len) {
        io_fputc(buf[i], f);
        i = i + 1;
    }
    if (io_fclose(f) != 0) return 0;
    return 1;
}
