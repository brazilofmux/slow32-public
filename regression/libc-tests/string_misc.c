/* HOSTLEG -- <string.h> beyond the first test's: the functions the
 * self-hosted library did not have (strnlen, strpbrk, strtok, strtok_r,
 * strcoll, strxfrm, strerror) and the corners of the ones it had --
 * bytes above 127 compare as unsigned char, a search for the terminator
 * finds it, overlapping moves in both directions. */
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <errno.h>

static int sign(int v) { return v < 0 ? -1 : v > 0; }

static void toks(const char *text, const char *delim) {
    char buf[64];
    char *t;
    strcpy(buf, text);
    printf("strtok[%s|%s]", text, delim);
    for (t = strtok(buf, delim); t; t = strtok(0, delim)) printf(" <%s>", t);
    printf("\n");
}

int main(void) {
    static char buf[128];
    char hi1[4], hi2[4];
    char *p, *save1, *save2, *o, *in;
    char outer[64], inner[32];
    int i;

    printf("strnlen %d %d %d %d\n", (int)strnlen("hello", 10), (int)strnlen("hello", 5),
           (int)strnlen("hello", 3), (int)strnlen("", 4));
    strcpy(buf, "key=value; other: thing");
    p = strpbrk(buf, ";:=");   printf("strpbrk %d\n", p ? (int)(p - buf) : -1);
    p = strpbrk(buf, ":");     printf("strpbrk %d\n", p ? (int)(p - buf) : -1);
    p = strpbrk(buf, "XYZ");   printf("strpbrk %d\n", p ? (int)(p - buf) : -1);
    p = strpbrk(buf, "");      printf("strpbrk %d\n", p ? (int)(p - buf) : -1);
    p = strpbrk("", "abc");    printf("strpbrk of empty %s\n", p ? "found" : "null");

    toks("a,b,c", ",");
    toks(",,a,,b,,", ",");
    toks("", ",");
    toks(",,,", ",");
    toks("one two\tthree", " \t");
    toks("nodelims", ",");
    toks("a,b;c", "");
    /* the delimiters may change between calls */
    strcpy(buf, "name=alpha beta;rest");
    p = strtok(buf, "=");   printf("strtok changing: <%s>", p);
    p = strtok(0, ";");     printf(" <%s>", p);
    p = strtok(0, "");      printf(" <%s>", p);
    p = strtok(0, "");      printf(" %s\n", p ? p : "(null)");

    /* strtok_r: two scans interleaved */
    strcpy(outer, "a=1,2;b=3;;c=4,5,6");
    for (o = strtok_r(outer, ";", &save1); o; o = strtok_r(0, ";", &save1)) {
        strcpy(inner, o);
        printf("strtok_r <%s>:", o);
        for (in = strtok_r(inner, "=,", &save2); in; in = strtok_r(0, "=,", &save2)) printf(" <%s>", in);
        printf("\n");
    }

    hi1[0] = 'a'; hi1[1] = (char)0x80; hi1[2] = 0;
    hi2[0] = 'a'; hi2[1] = 0x7f; hi2[2] = 0;
    printf("high bytes: strcmp %d strncmp %d memcmp %d strcoll %d\n", sign(strcmp(hi1, hi2)),
           sign(strncmp(hi1, hi2, 2)), sign(memcmp(hi1, hi2, 2)), sign(strcoll(hi1, hi2)));
    printf("high bytes: strcasecmp %d strncasecmp %d\n", sign(strcasecmp(hi1, hi2)), sign(strncasecmp(hi1, hi2, 2)));
    hi1[2] = (char)0xff; hi1[3] = 0;
    p = strchr(hi1, 0xff);         printf("strchr of 0xff: %d\n", p ? (int)(p - hi1) : -1);
    p = strchr(hi1, 0x1ff);        printf("strchr of 0x1ff (converted to char): %d\n", p ? (int)(p - hi1) : -1);
    p = strrchr(hi1, 0x80);        printf("strrchr of 0x80: %d\n", p ? (int)(p - hi1) : -1);
    p = memchr(hi1, 0xff, 4);      printf("memchr of 0xff: %d\n", p ? (int)(p - hi1) : -1);
    p = memchr(hi1, 0x180, 4);     printf("memchr of 0x180 (converted to unsigned char): %d\n", p ? (int)(p - hi1) : -1);
    p = memchr(hi1, 'a', 0);       printf("memchr in no bytes: %s\n", p ? "found" : "null");

    printf("strcoll %d %d %d\n", sign(strcoll("abc", "abd")), sign(strcoll("b", "a")), sign(strcoll("same", "same")));
    printf("strcasecmp %d %d %d %d\n", sign(strcasecmp("Hello", "hELLO")), sign(strcasecmp("abc", "ABD")),
           sign(strcasecmp("abc", "ab")), sign(strcasecmp("", "")));
    printf("strncasecmp %d %d %d\n", sign(strncasecmp("HELLO world", "hello THERE", 6)),
           sign(strncasecmp("HELLO world", "hello THERE", 7)), sign(strncasecmp("x", "Y", 0)));
    /* '[' lies between the cases: compared as lower case, "a" > "[" ... and "A" too */
    printf("strcasecmp across the gap %d %d\n", sign(strcasecmp("A", "[")), sign(strcasecmp("a", "[")));

    memset(buf, '#', 20); buf[20] = 0;
    i = (int)strxfrm(buf, "collate", 20);
    printf("strxfrm %d [%s]\n", i, buf);
    printf("strxfrm length only %d\n", (int)strxfrm(0, "four", 0));

    strcpy(buf, "0123456789");
    memmove(buf + 2, buf, 8);  printf("memmove up [%s]\n", buf);
    strcpy(buf, "0123456789");
    memmove(buf, buf + 3, 7);  printf("memmove down [%s]\n", buf);
    strcpy(buf, "0123456789");
    memmove(buf + 1, buf + 1, 5); memmove(buf, buf + 5, 0);  printf("memmove nothing [%s]\n", buf);

    memset(buf, 'x', 12); buf[12] = 0;
    strncpy(buf, "ab", 6);
    printf("strncpy pads:");
    for (i = 0; i < 8; i++) printf(" %d", buf[i]);
    printf("\n");
    strncpy(buf, "abcdefgh", 4);
    printf("strncpy does not terminate: %c%c%c%c then %d\n", buf[0], buf[1], buf[2], buf[3], buf[4]);
    strcpy(buf, "ab");
    strncat(buf, "cdefgh", 3);  printf("strncat [%s]\n", buf);
    strncat(buf, "XY", 10);     printf("strncat [%s]\n", buf);
    strncat(buf, "ZZ", 0);      printf("strncat [%s]\n", buf);

    printf("strspn %d %d %d strcspn %d %d %d\n", (int)strspn("aabbcc", "ab"), (int)strspn("xyz", "ab"),
           (int)strspn("abc", ""), (int)strcspn("hello, world", ",!"), (int)strcspn("hello", "xyz"), (int)strcspn("", "a"));
    p = strstr("needle in a haystack", "");     printf("strstr of empty: %s\n", p ? p : "(null)");
    p = strstr("aaab", "aab");                  printf("strstr %s\n", p ? p : "(null)");
    p = strstr("short", "longer than it");      printf("strstr %s\n", p ? p : "(null)");
    p = strstr("", "");                         printf("strstr both empty: %s\n", p ? "found" : "null");
    p = strdup("duplicate me");
    printf("strdup [%s] %d\n", p, (int)strlen(p));
    free(p);

    /* strerror: the text is the library's; that there is one is not */
    printf("strerror: %s %s %s\n", strerror(0) ? "text" : "NULL", strerror(ENOENT)[0] ? "text" : "EMPTY",
           strerror(99999) ? "text" : "NULL");
    printf("strerror tells errors apart: %d\n", strcmp(strerror(ENOENT), strerror(ENOMEM)) != 0);
    return 0;
}
