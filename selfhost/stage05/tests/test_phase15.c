/* Phase 15 test: varargs printf family (no float/64-bit) */

typedef char *va_list;

int snprintf(char *str, unsigned int size, const char *format, ...);
int sprintf(char *str, const char *format, ...);
int vsnprintf(char *str, unsigned int size, const char *format, va_list ap);
int fprintf(int fp, const char *format, ...);
int fopen(const char *path, const char *mode);
int fclose(int fp);
unsigned int fread(char *ptr, unsigned int size, unsigned int nmemb, int fp);
int remove(const char *path);

int my_strlen(char *s) {
    int n;
    n = 0;
    while (s[n] != 0) n = n + 1;
    return n;
}

int my_streq(char *a, char *b) {
    int i;
    i = 0;
    while (a[i] && b[i]) {
        if (a[i] != b[i]) return 0;
        i = i + 1;
    }
    return a[i] == b[i];
}

int my_vsnprintf(char *buf, unsigned int n, const char *fmt, ...) {
    va_list ap;
    int rc;
    va_start(ap, fmt);
    rc = vsnprintf(buf, n, fmt, ap);
    va_end(ap);
    return rc;
}

int test_sprintf_basic(void) {
    char buf[128];
    int rc;
    rc = sprintf(buf, "%d %u %x %X %o %c %s %%", -12, 34, 42, 42, 9, 'A', "ok");
    if (rc != my_strlen(buf)) return 1;
    if (!my_streq(buf, "-12 34 2a 2A 11 A ok %")) return 2;
    return 0;
}

int test_snprintf_trunc(void) {
    char buf[8];
    int rc;
    rc = snprintf(buf, 8, "hello-%d", 1234);
    if (rc != 10) return 1;
    if (!my_streq(buf, "hello-1")) return 2;
    return 0;
}

int test_vsnprintf(void) {
    char buf[32];
    int rc;
    rc = my_vsnprintf(buf, 32, "%d:%u:%x", -7, 99, 255);
    if (rc != my_strlen(buf)) return 1;
    if (!my_streq(buf, "-7:99:ff")) return 2;
    return 0;
}

int test_fprintf_file(void) {
    char buf[32];
    int fp;
    unsigned int n;
    int rc;
    fp = fopen("/tmp/s32-printf-test.txt", "w");
    if (!fp) return 1;
    rc = fprintf(fp, "N=%d U=%u", -5, 42);
    fclose(fp);
    if (rc != 9) return 2;
    fp = fopen("/tmp/s32-printf-test.txt", "r");
    if (!fp) return 3;
    n = fread(buf, 1, 31, fp);
    fclose(fp);
    remove("/tmp/s32-printf-test.txt");
    buf[n] = 0;
    if (!my_streq(buf, "N=-5 U=42")) return 4;
    return 0;
}

int main(void) {
    int rc;
    rc = test_sprintf_basic();
    if (rc) return 10 + rc;
    rc = test_snprintf_trunc();
    if (rc) return 20 + rc;
    rc = test_vsnprintf();
    if (rc) return 30 + rc;
    rc = test_fprintf_file();
    if (rc) return 40 + rc;
    return 0;
}
