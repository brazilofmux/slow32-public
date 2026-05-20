#include <stddef.h>

typedef struct later later_t;

typedef struct {
    int a;
    char *p;
} anon1_t;

typedef struct {
    int x;
    int y;
} anon2_t;

struct later {
    int a;
    int b;
    char *p;
    int c;
};

int main(void) {
    _Static_assert(offsetof(later_t, p) == 8, "p offset");
    _Static_assert(offsetof(later_t, c) == 16, "c offset");
    return 1;
}
