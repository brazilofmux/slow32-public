/* 7.14.1.1 signal: a handler may call signal for its own signal to stay installed, so repeated raises each reach it */
#include <stdio.h>
#include <signal.h>

static volatile sig_atomic_t count;

static void h(int s) {
    signal(s, h);
    count++;
}

int main(void) {
    int i, rs = 0;
    signal(SIGTERM, h);
    for (i = 0; i < 5; i++) rs += raise(SIGTERM);
    printf("calls %d, raise results sum %d\n", (int)count, rs);
    signal(SIGTERM, SIG_DFL);
    return 0;
}
