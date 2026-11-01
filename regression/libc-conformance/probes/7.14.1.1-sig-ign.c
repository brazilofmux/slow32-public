/* 7.14.1.1 signal: SIG_IGN makes a raised signal, SIGABRT included, do nothing; raise returns zero and the program continues */
#include <stdio.h>
#include <signal.h>

int main(void) {
    int r;
    signal(SIGTERM, SIG_IGN);
    r = raise(SIGTERM);
    printf("SIGTERM ignored, raise %d\n", r);
    signal(SIGABRT, SIG_IGN);
    r = raise(SIGABRT);
    printf("SIGABRT ignored, raise %d\n", r);
    signal(SIGFPE, SIG_IGN);
    r = raise(SIGFPE);
    printf("SIGFPE ignored, raise %d\n", r);
    return 0;
}
