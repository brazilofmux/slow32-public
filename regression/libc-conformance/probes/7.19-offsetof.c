/* 7.19p3: offsetof gives a size_t integer constant expression equal to the member's byte offset */
#include <stddef.h>
#include <stdio.h>

struct out { char c; double d; int arr[5]; short s; };

enum { E = offsetof(struct out, s) };
static char sized[offsetof(struct out, arr) + 1];

int main(void) {
    struct out o;
    char *base = (char *)&o;
    printf("%d %d %d %d\n", offsetof(struct out, c) == 0, (char *)&o.d - base == (ptrdiff_t)offsetof(struct out, d),
           (char *)o.arr - base == (ptrdiff_t)offsetof(struct out, arr), (char *)&o.s - base == E);
    printf("%d %d\n", sizeof sized == offsetof(struct out, arr) + 1, _Generic(offsetof(struct out, s), size_t: 1, default: 0));
    switch ((int)offsetof(struct out, c)) {
    case offsetof(struct out, c): printf("case\n"); break;
    }
    return 0;
}
