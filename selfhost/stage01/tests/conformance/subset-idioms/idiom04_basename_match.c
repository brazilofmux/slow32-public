static const char *basename_ptr(const char *path) {
    const char *p = path;
    const char *base = path;
    while (*p) {
        if (*p == '/' || *p == '\\') base = p + 1;
        p++;
    }
    return base;
}

static int str_eq(const char *a, const char *b) {
    int i = 0;
    for (;;) {
        if (a[i] != b[i]) return 0;
        if (a[i] == 0) return 1;
        i++;
    }
}

int main(void) {
    const char *a = basename_ptr("/tmp/member-a.src");
    const char *b = basename_ptr("member-b.src");
    if (!str_eq(a, "member-a.src")) return 1;
    if (!str_eq(b, "member-b.src")) return 2;
    return 0;
}
