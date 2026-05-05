#include <stddef.h>

struct pair {
    int x;
    long y;
};

int main(void) {
    _Static_assert(sizeof(struct pair) >= sizeof(long) + 4, "pair size");
    _Static_assert(offsetof(struct pair, x) == 0, "x offset");
    return 1;
}
