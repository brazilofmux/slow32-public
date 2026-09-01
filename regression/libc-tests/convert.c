/* strtol / atoi / snprintf: parsing and formatting edge cases, where
 * independent implementations most often drift. */
int printf(const char *fmt, ...);
int snprintf(char *b, unsigned int n, const char *fmt, ...);
long strtol(const char *s, char **end, int base);
int atoi(const char *s);
int strlen(const char *s);
int isspace(int c);
int isalpha(int c);
int isdigit(int c);
int isxdigit(int c);
int isprint(int c);
int ispunct(int c);
int toupper(int c);
int tolower(int c);

static char buf[64];

static void one(const char *s, int base) {
    char *e;
    long v;
    e = 0;
    v = strtol(s, &e, base);
    printf("strtol[%s,%d] = %ld consumed=%d\n", s, base, v,
           e != 0 ? (int)(e - (char *)s) : -1);
}

int main(void) {
    int i; int n;

    one("123", 10);
    one("  +42", 10);
    one("-7", 10);
    one("0x1f", 16);
    one("0x1f", 0);       /* base 0: prefix detection */
    one("010", 0);        /* base 0: octal */
    one("ff", 16);
    one("zzz", 10);       /* no digits: 0, consumed 0 */
    one("12abc", 10);     /* stops at 'a' */
    one("", 10);
    one("7fffffff", 16);
    one("-2147483648", 10);

    printf("atoi %d %d %d %d\n", atoi("42"), atoi("-42"), atoi("  9x"), atoi("q"));

    /* snprintf returns the length it WOULD have written */
    n = snprintf(buf, 8, "%s", "abcdefghij");
    printf("snprintf-trunc ret=%d buf=%s len=%d\n", n, buf, strlen(buf));
    n = snprintf(buf, 64, "%d,%x,%o,%c", -5, 255, 8, 'A');
    printf("snprintf-fmt ret=%d buf=%s\n", n, buf);
    n = snprintf(buf, 64, "%5d|%-5d|%05d", 42, 42, 42);
    printf("snprintf-width ret=%d buf=%s\n", n, buf);
    n = snprintf(buf, 64, "%u %ld", 4294967295u, 123456789L);
    printf("snprintf-long ret=%d buf=%s\n", n, buf);

    printf("ctype");
    for (i = 0; i < 128; i = i + 1) {
        int m;
        m = 0;
        if (isspace(i)) m = m + 1;
        if (isalpha(i)) m = m + 2;
        if (isdigit(i)) m = m + 4;
        if (isxdigit(i)) m = m + 8;
        if (isprint(i)) m = m + 16;
        if (ispunct(i)) m = m + 32;
        if (m) printf(" %d:%d", i, m);
    }
    printf("\n");
    printf("case %d %d %d %d\n", toupper('a'), tolower('Z'), toupper('1'), tolower('!'));
    printf("done\n");
    return 0;
}
