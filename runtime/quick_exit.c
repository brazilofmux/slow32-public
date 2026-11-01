/* quick_exit and at_quick_exit (C11 7.22.4.3, 7.22.4.7), for both C
 * libraries: the registered functions in the reverse of their order, then
 * _Exit -- no atexit handlers, and streams are not flushed. */
#include <stdlib.h>

#define QX_MAX 32       /* at least 32 registrations (7.22.4.3p3) */

static void (*qx_fn[QX_MAX])(void);
static int qx_n;

int at_quick_exit(void (*func)(void)) {
    if (qx_n >= QX_MAX) return -1;
    qx_fn[qx_n++] = func;
    return 0;
}

_Noreturn void quick_exit(int status) {
    while (qx_n > 0) qx_fn[--qx_n]();
    _Exit(status);
}
