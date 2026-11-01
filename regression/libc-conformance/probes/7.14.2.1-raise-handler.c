/* 7.14.2.1 raise: runs the handler with the signal number before returning, returns zero; afterwards the disposition is SIG_DFL or the handler */
#include <stdio.h>
#include <signal.h>

static volatile sig_atomic_t seen;
static volatile sig_atomic_t count;

static void h(int s) { seen = s; count++; }

int main(void) {
    int r, ok = 1;
    void (*p)(int);
    static const int sigs[] = { SIGABRT, SIGFPE, SIGILL, SIGINT, SIGSEGV, SIGTERM };
    int i;
    for (i = 0; i < 6; i++) {
        seen = 0;
        signal(sigs[i], h);
        r = raise(sigs[i]);
        if (r != 0 || seen != sigs[i]) ok = 0;
        p = signal(sigs[i], SIG_DFL);
        if (p != SIG_DFL && p != h) ok = 0;
    }
    printf("all six: %d, handler calls %d\n", ok, (int)count);
    return 0;
}
