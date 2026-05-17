static int strncmp_local(const char *a, const char *b, int n) {
    int i;
    for (i = 0; i < n; i++) {
        if (a[i] != b[i]) return 1;
        if (a[i] == 0) return 1;
    }
    return 0;
}

static int parse_reloc_expr(const char *s, const char *prefix, char *out, int out_sz) {
    int i = 0;
    int pfx_len = 4;
    if (strncmp_local(s, prefix, pfx_len) != 0) return 0;
    s += pfx_len;
    while (*s && *s != ')') {
        if (i + 1 >= out_sz) return 0;
        out[i++] = *s++;
    }
    if (*s != ')' || s[1] != 0) return 0;
    out[i] = 0;
    return i > 0;
}

int main(void) {
    char out[16];
    if (!parse_reloc_expr("%hi(sym)", "%hi(", out, 16)) return 1;
    if (out[0] != 's' || out[1] != 'y' || out[2] != 'm' || out[3] != 0) return 2;
    if (parse_reloc_expr("sym", "%hi(", out, 16)) return 3;
    return 0;
}
