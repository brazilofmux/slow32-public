/* 7.2.1.1p2: a false assert calls abort, which raises SIGABRT (7.22.4.1p2): a handler sees it */
#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

static void on_abrt(int sig) {
    (void)sig;
    _Exit(3);
}

int main(void) {
    int zero = 0;
    signal(SIGABRT, on_abrt);
    printf("before\n");
    fflush(stdout);
    assert(zero);
    printf("after\n");
    return 0;
}
