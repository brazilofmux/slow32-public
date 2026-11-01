/* offsetof with a nested member and array elements in its designator
 * (7.19p3; libc-conformance 7.19-offsetof-designators): each must equal
 * the distance the same path has inside a real object. */
#include <stddef.h>
struct in { char a; double b; int c[3][4]; };
struct out { char c; struct in i[2]; int tail; };
int main(void) {
    struct out o;
    char *base = (char *)&o;
    if (offsetof(struct out, i[1].c[2][3]) != (size_t)((char *)&o.i[1].c[2][3] - base)) return 1;
    if (offsetof(struct out, i[1].b) != (size_t)((char *)&o.i[1].b - base)) return 2;
    if (offsetof(struct out, i[0].c[1]) != (size_t)((char *)&o.i[0].c[1] - base)) return 3;
    if (offsetof(struct out, tail) != (size_t)((char *)&o.tail - base)) return 4;
    return 0;
}
