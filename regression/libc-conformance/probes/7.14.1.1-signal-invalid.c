/* 7.14.1.1 signal: a request that cannot be honoured (no such signal number) returns SIG_ERR and stores a positive errno */
#include <stdio.h>
#include <errno.h>
#include <signal.h>

static void h(int s) { (void)s; }

int main(void) {
    void (*p)(int);
    errno = 0;
    p = signal(-1, h);
    printf("-1: %d %d ", p == SIG_ERR, errno > 0);
    errno = 0;
    p = signal(9999, h);
    printf("9999: %d %d ", p == SIG_ERR, errno > 0);
    errno = 0;
    p = signal(9999, SIG_DFL);
    printf("9999 SIG_DFL: %d %d\n", p == SIG_ERR, errno > 0);
    return 0;
}
