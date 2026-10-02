/* HOSTLEG -- <stdlib.h> where the answer does not depend on the width
 * of long: abs and friends, the ato* family, div, the 64-bit strto*
 * limits, strtol's handling of prefixes and of what it cannot convert,
 * qsort, bsearch, the allocator's contract, atexit's order. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void tl(const char *s, int base) {
    char *e = 0;
    long v;
    errno = 0;
    v = strtol(s, &e, base);
    printf("strtol[%s,%d] = %ld consumed=%d erange=%d\n", s, base, v, (int)(e - s), errno == ERANGE);
}

static void tul(const char *s, int base) {
    char *e = 0;
    unsigned long v;
    errno = 0;
    v = strtoul(s, &e, base);
    printf("strtoul[%s,%d] = %lu consumed=%d erange=%d\n", s, base, v, (int)(e - s), errno == ERANGE);
}

static void tll(const char *s, int base) {
    char *e = 0;
    long long v;
    errno = 0;
    v = strtoll(s, &e, base);
    printf("strtoll[%s,%d] = %lld consumed=%d erange=%d\n", s, base, v, (int)(e - s), errno == ERANGE);
}

static void tull(const char *s, int base) {
    char *e = 0;
    unsigned long long v;
    errno = 0;
    v = strtoull(s, &e, base);
    printf("strtoull[%s,%d] = %llu consumed=%d erange=%d\n", s, base, v, (int)(e - s), errno == ERANGE);
}

static int cmp_int(const void *a, const void *b) {
    int x = *(const int *)a, y = *(const int *)b;
    return x < y ? -1 : x > y;
}

static int cmp_str(const void *a, const void *b) {
    return strcmp(*(char *const *)a, *(char *const *)b);
}

struct rec { int key; int seq; };
static int cmp_rec(const void *a, const void *b) {
    const struct rec *x = a, *y = b;
    if (x->key != y->key) return x->key < y->key ? -1 : 1;
    return x->seq < y->seq ? -1 : x->seq > y->seq;       /* total order: no ties left to chance */
}

static void bye1(void) { printf("atexit 1 (registered first, runs last)\n"); }
static void bye2(void) { printf("atexit 2\n"); }
static void bye3(void) { printf("atexit 3 (registered last, runs first)\n"); }

int main(void) {
    static int big[3000];
    static struct rec recs[500];
    int a[9] = {5, -3, 9, 0, 5, 2147483647, -2147483647 - 1, 1, 5};
    const char *words[7] = {"pear", "apple", "fig", "", "apple", "Zebra", "banana"};
    int i, bad, key;
    unsigned int seed;
    int *hit;
    div_t d;
    ldiv_t ld;
    char *p, *q;

    printf("abs %d %d %d\n", abs(-7), abs(7), abs(0));
    printf("labs %ld %ld\n", labs(-123456L), labs(99L));
    printf("llabs %lld %lld\n", llabs(-9000000000LL), llabs(9000000000LL));

    printf("atoi %d %d %d %d %d %d\n", atoi("42"), atoi("  -17"), atoi("\t\n\v\f\r 99x"),
           atoi("+5"), atoi("abc"), atoi("-0"));
    printf("atoi %d %d\n", atoi("2147483647"), atoi("-2147483648"));
    printf("atol %ld %ld %ld\n", atol(" \t123456"), atol("-98765 4"), atol(""));
    printf("atoll %lld %lld\n", atoll("  9007199254740993"), atoll("-9223372036854775807"));

    d = div(17, 5);    printf("div %d %d\n", d.quot, d.rem);
    d = div(-17, 5);   printf("div %d %d\n", d.quot, d.rem);
    d = div(17, -5);   printf("div %d %d\n", d.quot, d.rem);
    d = div(-17, -5);  printf("div %d %d\n", d.quot, d.rem);
    ld = ldiv(1000000L, 7L);   printf("ldiv %ld %ld\n", ld.quot, ld.rem);
    ld = ldiv(-1000000L, 7L);  printf("ldiv %ld %ld\n", ld.quot, ld.rem);

    tl("0", 10); tl("  +12abc", 10); tl("-12", 10); tl("", 10); tl("   ", 10); tl("+", 10); tl("-", 0);
    tl("0x1F", 16); tl("0x1F", 0); tl("1F", 16); tl("0x", 16); tl("0x", 0); tl("0xg", 0); tl("0X7f", 0);
    tl("017", 0); tl("08", 0); tl("019", 8); tl("zz", 36); tl("Zz", 36); tl("z", 35); tl("101", 2);
    tl("12", 1); tl("12", 37); tl("\v\f\r\n\t 7", 10); tl("- 5", 10); tl("--5", 10); tl("0x-5", 16);
    tl("2147483647", 10); tl("-2147483648", 10); tl("7fffffff", 16); tl("-0x80000000", 0);
    tul("0", 10); tul("4294967295", 10); tul("0xffffffff", 0); tul("  077", 0); tul("12ab", 10); tul("", 10);
    tul("0x", 0); tul("+9", 10);

    tll("9223372036854775807", 10); tll("9223372036854775808", 10); tll("-9223372036854775808", 10);
    tll("-9223372036854775809", 10); tll("99999999999999999999999", 10); tll("0x7fffffffffffffff", 0);
    tll("  -42tail", 10); tll("0x", 0); tll("", 10);
    tull("18446744073709551615", 10); tull("18446744073709551616", 10); tull("0xffffffffffffffff", 16);
    tull("-1", 10); tull("777", 8); tull("x", 10);

    qsort(a, 9, sizeof a[0], cmp_int);
    printf("qsort");
    for (i = 0; i < 9; i++) printf(" %d", a[i]);
    printf("\n");
    qsort(words, 7, sizeof words[0], cmp_str);
    printf("qsort");
    for (i = 0; i < 7; i++) printf(" [%s]", words[i]);
    printf("\n");
    qsort(a, 0, sizeof a[0], cmp_int);
    qsort(a, 1, sizeof a[0], cmp_int);
    printf("qsort of none and of one: %d\n", a[0]);

    seed = 12345;
    for (i = 0; i < 3000; i++) {
        seed = seed * 1103515245u + 12345u;
        big[i] = (int)((seed >> 8) % 1000u) - 500;      /* many duplicates */
    }
    qsort(big, 3000, sizeof big[0], cmp_int);
    bad = 0;
    for (i = 1; i < 3000; i++) if (big[i - 1] > big[i]) bad++;
    printf("qsort 3000: out of order %d, first %d last %d middle %d\n", bad, big[0], big[2999], big[1500]);
    for (i = 0; i < 3000; i++) big[i] = 3000 - i;        /* descending */
    qsort(big, 3000, sizeof big[0], cmp_int);
    bad = 0;
    for (i = 0; i < 3000; i++) if (big[i] != i + 1) bad++;
    printf("qsort descending: wrong %d\n", bad);
    for (i = 0; i < 500; i++) { seed = seed * 1103515245u + 12345u; recs[i].key = (int)((seed >> 16) % 13u); recs[i].seq = i; }
    qsort(recs, 500, sizeof recs[0], cmp_rec);
    bad = 0;
    for (i = 1; i < 500; i++) if (cmp_rec(&recs[i - 1], &recs[i]) >= 0) bad++;
    printf("qsort structs: out of order %d, first %d/%d last %d/%d\n", bad,
           recs[0].key, recs[0].seq, recs[499].key, recs[499].seq);

    for (i = 0; i < 3000; i++) big[i] = i * 3;
    key = 2997;  hit = bsearch(&key, big, 3000, sizeof big[0], cmp_int);  printf("bsearch 2997: %d\n", hit ? (int)(hit - big) : -1);
    key = 0;     hit = bsearch(&key, big, 3000, sizeof big[0], cmp_int);  printf("bsearch 0: %d\n", hit ? (int)(hit - big) : -1);
    key = 8997;  hit = bsearch(&key, big, 3000, sizeof big[0], cmp_int);  printf("bsearch 8997: %d\n", hit ? (int)(hit - big) : -1);
    key = 4;     hit = bsearch(&key, big, 3000, sizeof big[0], cmp_int);  printf("bsearch 4: %d\n", hit ? (int)(hit - big) : -1);
    key = -1;    hit = bsearch(&key, big, 3000, sizeof big[0], cmp_int);  printf("bsearch -1: %d\n", hit ? (int)(hit - big) : -1);
    key = 9000;  hit = bsearch(&key, big, 3000, sizeof big[0], cmp_int);  printf("bsearch 9000: %d\n", hit ? (int)(hit - big) : -1);
    key = 3;     hit = bsearch(&key, big, 0, sizeof big[0], cmp_int);     printf("bsearch in none: %d\n", hit ? 1 : 0);

    /* the same seed gives the same sequence; no srand at all is srand(1) */
    {
        int first[8], same = 1, inrange = 1, differ = 0;
        for (i = 0; i < 8; i++) { first[i] = rand(); if (first[i] < 0 || first[i] > RAND_MAX) inrange = 0; }
        srand(1);
        for (i = 0; i < 8; i++) if (rand() != first[i]) same = 0;
        srand(2);
        for (i = 0; i < 8; i++) if (rand() != first[i]) differ = 1;
        printf("rand: default is srand(1) %d, in range %d, another seed differs %d, RAND_MAX>=32767 %d\n",
               same, inrange, differ, RAND_MAX >= 32767);
    }
    /* a generator worth the name reaches the top half of its range */
    {
        int top = 0;
        srand(7);
        for (i = 0; i < 1000; i++) if (rand() > RAND_MAX / 2) top++;
        printf("rand: above half of RAND_MAX %s\n", top > 300 && top < 700 ? "about half the time" : "NOT half the time");
    }

    p = calloc(100, 7);
    bad = 0;
    for (i = 0; i < 700; i++) if (p[i]) bad++;
    printf("calloc: nonzero bytes %d\n", bad);
    for (i = 0; i < 700; i++) p[i] = (char)(i * 7);
    q = realloc(p, 70000);
    bad = 0;
    for (i = 0; i < 700; i++) if (q[i] != (char)(i * 7)) bad++;
    printf("realloc larger: changed bytes %d\n", bad);
    p = realloc(q, 10);
    bad = 0;
    for (i = 0; i < 10; i++) if (p[i] != (char)(i * 7)) bad++;
    printf("realloc smaller: changed bytes %d\n", bad);
    free(p);
    free(0);
    p = realloc(0, 32);
    printf("realloc(0, n): %s\n", p ? "allocates" : "FAILS");
    free(p);
    printf("getenv of a name not set: %s\n", getenv("S32_LIBC_TEST_NO_SUCH_NAME") ? "SET?" : "null");

    printf("atexit %d %d %d\n", atexit(bye1), atexit(bye2), atexit(bye3));
    printf("main returns\n");
    return 0;
}
