/* 7.2p1, 7.1.2p4: each inclusion of <assert.h> redefines assert by the current state of NDEBUG */
#include <stdio.h>
#include <assert.h>

static int calls;
static int bump(void) { return ++calls; }

#define NDEBUG
#include <assert.h>
static void off(void) { assert(bump()); }

#undef NDEBUG
#include <assert.h>
static void on(void) { assert(bump()); }

int main(void) {
    off();
    printf("off %d\n", calls);
    on();
    printf("on %d\n", calls);
    return 0;
}
