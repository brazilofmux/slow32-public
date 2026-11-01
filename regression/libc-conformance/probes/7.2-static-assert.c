/* 7.2p3: <assert.h> defines static_assert, which expands to _Static_assert */
#include <assert.h>
#include <stdio.h>

static_assert(sizeof(int) >= 2, "int has at least 16 bits");

int main(void) {
#ifdef static_assert
    puts("macro");
#endif
    return 0;
}
