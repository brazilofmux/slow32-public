/* 7.1.2 standard headers: the small headers in reverse order, each included twice, as if once */
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdalign.h>
#include <setjmp.h>
#include <limits.h>
#include <inttypes.h>
#include <errno.h>
#include <ctype.h>
#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdalign.h>
#include <setjmp.h>
#include <limits.h>
#include <inttypes.h>
#include <errno.h>
#include <ctype.h>
#include <assert.h>
#include <stdio.h>

int main(void) {
    int32_t a = INT32_MAX;
    size_t n = offsetof(struct { char c; int i; }, i);
    bool b = true;
    jmp_buf env;
    (void)env;
    assert(b);
    printf("%d %d %d %d\n", a == INT_MAX, n >= 1, isdigit('7') != 0, errno == 0);
    return 0;
}
