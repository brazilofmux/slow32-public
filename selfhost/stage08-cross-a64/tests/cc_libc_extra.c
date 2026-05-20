/* Smoke test for libc_extra.c additions: snprintf / strtoul / strtok_r /
   qsort / memmove / memchr / __builtin_ctz / __builtin_popcount.
   Returns 1 on full success, distinct numeric codes per failed check. */

int   snprintf(char *buf, int size, char *fmt, ...);
unsigned long strtoul(char *s, char **endptr, int base);
char *strtok_r(char *s, char *delim, char **saveptr);
void  qsort(char *base, int n, int size, int (*cmp)());
char *memmove(char *dst, char *src, int n);
char *memchr(char *s, int c, int n);
int   __builtin_ctz(unsigned int x);
int   __builtin_popcount(unsigned int x);
int   strcmp(char *a, char *b);

static int cmp_int(int *a, int *b) { return *a - *b; }

int main(void) {
    /* snprintf */
    char buf[64];
    int n = snprintf(buf, 64, "x=%d hex=%x str=%s", 42, 255, "hi");
    if (n != 18) return 10;
    if (strcmp(buf, "x=42 hex=ff str=hi") != 0) return 11;

    /* strtoul: base 0 with hex prefix, decimal, octal */
    char *end;
    if (strtoul("0xff", &end, 0) != 255) return 20;
    if (*end != 0) return 21;
    if (strtoul("123abc", &end, 10) != 123) return 22;
    if (*end != 'a') return 23;
    if (strtoul("017", &end, 0) != 15) return 24;

    /* memchr */
    char *p = memchr("hello", 'l', 5);
    if (!p || *p != 'l' || p[1] != 'l') return 30;
    if (memchr("hi", 'z', 2) != 0) return 31;

    /* memmove (overlapping forward: dst > src) */
    char m[8];
    m[0] = 'a'; m[1] = 'b'; m[2] = 'c'; m[3] = 'd'; m[4] = 'e';
    memmove(m + 1, m, 4);
    if (m[0] != 'a' || m[1] != 'a' || m[2] != 'b' || m[3] != 'c' || m[4] != 'd')
        return 40;

    /* __builtin_ctz / popcount */
    if (__builtin_ctz(0x10) != 4) return 50;
    if (__builtin_ctz(0x1) != 0) return 51;
    if (__builtin_popcount(0xF0F) != 8) return 52;
    if (__builtin_popcount(0) != 0) return 53;

    /* strtok_r — destructive */
    char str[16];
    char *save;
    str[0] = 'a'; str[1] = ','; str[2] = 'b'; str[3] = ',';
    str[4] = ',';  str[5] = 'c'; str[6] = 0;
    char *t1 = strtok_r(str, ",", &save);
    if (!t1 || strcmp(t1, "a") != 0) return 60;
    char *t2 = strtok_r(0, ",", &save);
    if (!t2 || strcmp(t2, "b") != 0) return 61;
    char *t3 = strtok_r(0, ",", &save);
    if (!t3 || strcmp(t3, "c") != 0) return 62;
    char *t4 = strtok_r(0, ",", &save);
    if (t4 != 0) return 63;

    /* qsort */
    int arr[5];
    arr[0] = 5; arr[1] = 2; arr[2] = 4; arr[3] = 1; arr[4] = 3;
    qsort((char *)arr, 5, sizeof(int), cmp_int);
    if (arr[0] != 1) return 70;
    if (arr[1] != 2) return 71;
    if (arr[2] != 3) return 72;
    if (arr[3] != 4) return 73;
    if (arr[4] != 5) return 74;

    return 1;
}
