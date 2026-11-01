/* 7.14.1.1 signal: each call returns the handler the previous call installed for that signal, SIG_IGN and SIG_DFL included */
#include <stdio.h>
#include <signal.h>

static void h1(int s) { (void)s; }
static void h2(int s) { (void)s; }

int main(void) {
    void (*p)(int);
    p = signal(SIGTERM, h1);
    printf("startup %d ", p == SIG_DFL || p == SIG_IGN);
    p = signal(SIGTERM, h2);
    printf("%d ", p == h1);
    p = signal(SIGTERM, SIG_IGN);
    printf("%d ", p == h2);
    p = signal(SIGTERM, SIG_DFL);
    printf("%d ", p == SIG_IGN);
    p = signal(SIGTERM, h1);
    printf("%d ", p == SIG_DFL);
    p = signal(SIGFPE, h2);
    printf("independent %d ", p != h1);
    p = signal(SIGTERM, SIG_DFL);
    printf("%d\n", p == h1);
    return 0;
}
