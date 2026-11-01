/* 7.2p1: with NDEBUG defined before <assert.h>, assert does not evaluate its argument and a false one does nothing */
#define NDEBUG
#include <assert.h>
#include <stdio.h>

static int calls;
static int bump(void) { calls++; return 0; }

int main(void) {
    assert(bump());
    assert(0);
    printf("calls %d\n", calls);
    return 0;
}
