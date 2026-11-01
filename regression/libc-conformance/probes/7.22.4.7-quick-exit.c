/* 7.22.4.3 at_quick_exit, 7.22.4.7 quick_exit: its own handlers run in reverse order, atexit ones do not */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void a(void) { printf("qa\n"); fflush(stdout); }
static void b(void) { printf("qb\n"); fflush(stdout); }
static void c(void) { printf("atexit must not run\n"); }
int main(void) {
    atexit(c);
    printf("%d %d\n", at_quick_exit(a) == 0, at_quick_exit(b) == 0);
    fflush(stdout);
    quick_exit(6);
}
