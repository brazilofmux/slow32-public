/* test_phase22.c -- 64-bit printf and conversion tests */
int write(int fd, char *buf, int len);
int sprintf(char *str, const char *format, ...);
int slow32_utoa64(unsigned long long val, char *buf);
int slow32_ltoa64(long long val, char *buf);
int slow32_utox64(unsigned long long val, char *buf, int upper);
int slow32_utoo64(unsigned long long val, char *buf);

int my_strlen(char *s) {
    int n;
    n = 0;
    while (*(s + n) != 0) {
        n = n + 1;
    }
    return n;
}

void my_puts(char *s) {
    write(1, s, my_strlen(s));
}

int my_strcmp(char *a, char *b) {
    while (*a && *b && *a == *b) {
        a = a + 1;
        b = b + 1;
    }
    return *a - *b;
}

int test_count;
int fail_count;

void check(int cond, char *msg) {
    test_count = test_count + 1;
    if (!cond) {
        fail_count = fail_count + 1;
        my_puts("FAIL: ");
        my_puts(msg);
        my_puts("\n");
    }
}

void check_str(char *got, char *expect, char *msg) {
    test_count = test_count + 1;
    if (my_strcmp(got, expect) != 0) {
        fail_count = fail_count + 1;
        my_puts("FAIL: ");
        my_puts(msg);
        my_puts(" got='");
        my_puts(got);
        my_puts("' expect='");
        my_puts(expect);
        my_puts("'\n");
    }
}

int main(void) {
    char buf[64];
    long long v64;
    unsigned long long uv64;

    test_count = 0;
    fail_count = 0;

    /* --- sprintf %lld tests --- */
    sprintf(buf, "%lld", (long long)0);
    check_str(buf, "0", "sprintf %lld 0");

    sprintf(buf, "%lld", (long long)42);
    check_str(buf, "42", "sprintf %lld 42");

    sprintf(buf, "%lld", (long long)-1);
    check_str(buf, "-1", "sprintf %lld -1");

    sprintf(buf, "%lld", (long long)-123);
    check_str(buf, "-123", "sprintf %lld -123");

    sprintf(buf, "%lld", (long long)1000000);
    check_str(buf, "1000000", "sprintf %lld 1000000");

    /* --- sprintf %llu tests --- */
    sprintf(buf, "%llu", (unsigned long long)0);
    check_str(buf, "0", "sprintf %llu 0");

    sprintf(buf, "%llu", (unsigned long long)42);
    check_str(buf, "42", "sprintf %llu 42");

    sprintf(buf, "%llu", (unsigned long long)1000000);
    check_str(buf, "1000000", "sprintf %llu 1000000");

    /* --- sprintf %llx tests --- */
    sprintf(buf, "%llx", (unsigned long long)0);
    check_str(buf, "0", "sprintf %llx 0");

    sprintf(buf, "%llx", (unsigned long long)255);
    check_str(buf, "ff", "sprintf %llx 255");

    sprintf(buf, "%llx", (unsigned long long)65535);
    check_str(buf, "ffff", "sprintf %llx 65535");

    sprintf(buf, "%llX", (unsigned long long)255);
    check_str(buf, "FF", "sprintf %llX 255");

    /* --- sprintf %llo tests --- */
    sprintf(buf, "%llo", (unsigned long long)0);
    check_str(buf, "0", "sprintf %llo 0");

    sprintf(buf, "%llo", (unsigned long long)8);
    check_str(buf, "10", "sprintf %llo 8");

    sprintf(buf, "%llo", (unsigned long long)255);
    check_str(buf, "377", "sprintf %llo 255");

    /* --- Mixed 32/64-bit formats --- */
    sprintf(buf, "%d %lld", 42, (long long)99);
    check_str(buf, "42 99", "mixed %d %lld");

    sprintf(buf, "%lld %d", (long long)99, 42);
    check_str(buf, "99 42", "mixed %lld %d");

    sprintf(buf, "%u %llu", (unsigned int)10, (unsigned long long)20);
    check_str(buf, "10 20", "mixed %u %llu");

    /* --- Edge cases --- */
    sprintf(buf, "%lld", (long long)-1);
    check_str(buf, "-1", "sprintf %lld -1 edge");

    /* --- slow32_utoa64 tests --- */
    slow32_utoa64((unsigned long long)0, buf);
    check_str(buf, "0", "utoa64 0");

    slow32_utoa64((unsigned long long)42, buf);
    check_str(buf, "42", "utoa64 42");

    slow32_utoa64((unsigned long long)1000000, buf);
    check_str(buf, "1000000", "utoa64 1000000");

    /* --- slow32_ltoa64 tests --- */
    slow32_ltoa64((long long)0, buf);
    check_str(buf, "0", "ltoa64 0");

    slow32_ltoa64((long long)42, buf);
    check_str(buf, "42", "ltoa64 42");

    slow32_ltoa64((long long)-1, buf);
    check_str(buf, "-1", "ltoa64 -1");

    slow32_ltoa64((long long)-123, buf);
    check_str(buf, "-123", "ltoa64 -123");

    /* --- slow32_utox64 tests --- */
    slow32_utox64((unsigned long long)0, buf, 0);
    check_str(buf, "0", "utox64 0");

    slow32_utox64((unsigned long long)255, buf, 0);
    check_str(buf, "ff", "utox64 255");

    slow32_utox64((unsigned long long)255, buf, 1);
    check_str(buf, "FF", "utox64 255 upper");

    /* --- slow32_utoo64 tests --- */
    slow32_utoo64((unsigned long long)0, buf);
    check_str(buf, "0", "utoo64 0");

    slow32_utoo64((unsigned long long)8, buf);
    check_str(buf, "10", "utoo64 8");

    slow32_utoo64((unsigned long long)255, buf);
    check_str(buf, "377", "utoo64 255");

    /* --- Summary --- */
    if (fail_count == 0) {
        my_puts("test_phase22: all tests passed\n");
    } else {
        my_puts("test_phase22: FAILURES\n");
    }
    return fail_count;
}
