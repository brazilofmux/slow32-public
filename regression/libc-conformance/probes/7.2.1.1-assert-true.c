/* 7.2.1.1 assert: a true scalar argument (integer, pointer, double) is evaluated once and execution continues; assert is a void expression */
#include <assert.h>
#include <stdio.h>

static int calls;
static int bump(void) { return ++calls; }

int main(void) {
    int x = 5;
    const char *p = "s";
    double d = 0.5;
    assert(bump());
    assert(p);
    assert(d);
    assert(x == 5), x++;
    (void)(1 ? assert(x > 0) : (void)0);
    printf("calls %d x %d\n", calls, x);
    return 0;
}
