/* String and memory routines: the two libcs implement these
 * independently, so every line here is a chance for them to disagree.
 * Edge cases first -- that is where independent implementations drift. */
int printf(const char *fmt, ...);
int strcmp(const char *a, const char *b);
int strncmp(const char *a, const char *b, int n);
char *strcpy(char *d, const char *s);
char *strncpy(char *d, const char *s, int n);
char *strcat(char *d, const char *s);
char *strncat(char *d, const char *s, int n);
char *strchr(const char *s, int c);
char *strrchr(const char *s, int c);
char *strstr(const char *h, const char *n);
int strlen(const char *s);
int memcmp(const void *a, const void *b, int n);
char *memchr(const void *s, int c, int n);
char *memcpy(char *d, const char *s, int n);
char *memset(char *d, int c, int n);

static char buf[64];
static char b2[64];

/* sign only: implementations may return any negative/positive value */
static int sgn(int v) { if (v < 0) return -1; if (v > 0) return 1; return 0; }

int main(void) {
    int i;

    printf("strcmp %d %d %d\n", sgn(strcmp("abc","abc")),
           sgn(strcmp("abc","abd")), sgn(strcmp("abd","abc")));
    printf("strcmp-empty %d %d\n", sgn(strcmp("","")), sgn(strcmp("","a")));
    printf("strncmp %d %d %d\n", sgn(strncmp("abc","abd",2)),
           sgn(strncmp("abc","abd",3)), sgn(strncmp("abc","abd",0)));

    /* strncpy pads the remainder with NULs and does NOT terminate on overflow */
    for (i = 0; i < 8; i = i + 1) buf[i] = 'Z';
    strncpy(buf, "ab", 6);
    printf("strncpy-pad");
    for (i = 0; i < 8; i = i + 1) printf(" %d", buf[i]);
    printf("\n");
    for (i = 0; i < 8; i = i + 1) buf[i] = 'Z';
    strncpy(buf, "abcdef", 3);
    printf("strncpy-trunc %d %d %d %d\n", buf[0], buf[1], buf[2], buf[3]);

    strcpy(buf, "foo");
    strcat(buf, "bar");
    printf("strcat %s %d\n", buf, strlen(buf));
    strcpy(buf, "foo");
    strncat(buf, "barbaz", 3);
    printf("strncat %s %d\n", buf, strlen(buf));

    /* Offsets are taken against ONE buffer.  Doing pointer arithmetic
     * between two occurrences of the same string literal is not
     * portable: identical literals may or may not be merged, and the
     * two compilers here differ (clang merges, stage08 cc does not). */
    strcpy(buf, "hello");
    {
        char *p;
        p = strchr(buf, 'l');
        printf("strchr %d %d %d\n", p != 0 ? (int)(p - buf) : -1,
               strchr(buf, 'z') == 0 ? 1 : 0,
               strchr(buf, 0) != 0 ? 1 : 0);   /* finds the terminator */
        p = strrchr(buf, 'l');
        printf("strrchr %d %d\n", p != 0 ? (int)(p - buf) : -1,
               strrchr(buf, 'z') == 0 ? 1 : 0);
        p = strstr(buf, "ll");
        printf("strstr %d %d %d\n", p != 0 ? (int)(p - buf) : -1,
               strstr(buf, "zz") == 0 ? 1 : 0,
               strstr(buf, "") != 0 ? 1 : 0);  /* empty needle matches */
    }

    printf("memcmp %d %d %d\n", sgn(memcmp("abc","abc",3)),
           sgn(memcmp("abc","abd",3)), sgn(memcmp("abc","abd",0)));
    memcpy(b2, "hello", 6);
    printf("memchr %d %d\n",
           memchr(b2, 'l', 6) != 0 ? (int)(memchr(b2,'l',6) - b2) : -1,
           memchr(b2, 'z', 6) == 0 ? 1 : 0);

    /* overlapping-ish and zero-length forms */
    memset(buf, 'x', 0);
    memcpy(buf, "q", 0);
    memset(b2, 0x41, 5); b2[5] = 0;
    printf("memset %s\n", b2);
    printf("done\n");
    return 0;
}
