/* assert.c -- what a failing assert calls (<assert.h>), for both C
 * libraries: the expression, the function, the file and the line, on
 * stderr (7.2.1.1p2), then abort. */
#include <stdio.h>
#include <stdlib.h>

void __s32_assert_func(const char *expr, const char *file, int line, const char *func) {
    fprintf(stderr, "Assertion failed: %s, function %s, file %s, line %d.\n", expr, func, file, line);
    fflush(stderr);
    abort();
}
