/* 7.14 signal.h: a handler sets a volatile sig_atomic_t flag that the main program then sees; the six signal macros are distinct positive constants */
#include <stdio.h>
#include <signal.h>

static volatile sig_atomic_t flag;

static void h(int s) { (void)s; flag = 1; }

int main(void) {
    int v[6] = { SIGABRT, SIGFPE, SIGILL, SIGINT, SIGSEGV, SIGTERM };
    int i, j, ok = 1, spins = 0;
    for (i = 0; i < 6; i++) {
        if (v[i] <= 0) ok = 0;
        for (j = i + 1; j < 6; j++) if (v[i] == v[j]) ok = 0;
    }
    printf("distinct positive %d\n", ok);
    signal(SIGINT, h);
    raise(SIGINT);
    while (!flag && spins < 1000) spins++;
    printf("flag %d\n", (int)flag);
    signal(SIGINT, SIG_DFL);
    return 0;
}
