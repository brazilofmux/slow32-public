/* 7.21.6.2 fscanf: %[ scansets, negated scansets, a leading ] as a member, and - first or last as a literal */
#include <stdio.h>

int main(void) {
    char a[16], b[16], c[16];
    int r = sscanf("abcbaxyz", "%[abc]%s", a, b);
    printf("%d: %s %s\n", r, a, b);
    r = sscanf("key=value,rest", "%[^=]=%[^,],%s", a, b, c);
    printf("%d: %s %s %s\n", r, a, b, c);
    r = sscanf("]]a]b", "%[]a]%s", a, b);
    printf("%d: %s %s\n", r, a, b);
    r = sscanf("ab]cd", "%[^]]]%s", a, b);
    printf("%d: %s %s\n", r, a, b);
    r = sscanf("--a-b+c", "%[-ab]%s", a, b);
    printf("%d: %s %s\n", r, a, b);
    r = sscanf("+x-y", "%[+x-]%s", a, b);
    printf("%d: %s %s\n", r, a, b);
    r = sscanf("   lead", "%[ ]%s", a, b);
    printf("%d: [%s] %s\n", r, a, b);
    r = sscanf("abcdef", "%3[abcdef]", a);
    printf("%d: %s\n", r, a);
    r = sscanf("xyz", "%[abc]", a);
    printf("%d\n", r);
    return 0;
}
