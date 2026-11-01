/* 7.24.5.7 strstr: an empty needle returns s1, partial matches restart correctly, the needle longer than s1 is absent */
#include <stdio.h>
#include <string.h>

static long at(const char *p, const char *base) { return p ? (long)(p - base) : -1L; }

int main(void) {
    const char s[] = "aaabaaab\xe9\xe9x";
    const char e[] = "";
    printf("%ld %ld ", at(strstr(s, ""), s), at(strstr(e, ""), e));
    printf("%ld %ld %ld ", at(strstr(s, "aab"), s), at(strstr(s, "baaab"), s), at(strstr(s, "abab"), s));
    printf("%ld %ld %ld ", at(strstr(s, "\xe9x"), s), at(strstr("ab", "abc"), "ab"), at(strstr(e, "a"), e));
    printf("%ld %ld\n", at(strstr("abcabcabd", "abcabd"), "abcabcabd"), at(strstr(s, s), s));
    return 0;
}
