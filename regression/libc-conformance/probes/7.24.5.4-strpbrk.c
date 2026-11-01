/* 7.24.5.4 strpbrk: first character of s1 found in s2, the terminator never matches, high bytes */
#include <stdio.h>
#include <string.h>

static long at(const char *p, const char *base) { return p ? (long)(p - base) : -1L; }

int main(void) {
    const char s[] = "hello, w\xe9rld";
    printf("%ld %ld %ld ", at(strpbrk(s, "ol"), s), at(strpbrk(s, " ,"), s), at(strpbrk(s, "xyz"), s));
    printf("%ld %ld %ld\n", at(strpbrk(s, ""), s), at(strpbrk("", "a"), ""), at(strpbrk(s, "\xe9"), s));
    return 0;
}
