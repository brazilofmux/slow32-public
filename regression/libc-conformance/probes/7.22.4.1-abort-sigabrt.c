/* 7.22.4.1 abort: raises SIGABRT, so a handler for it runs */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <signal.h>

static void on_abrt(int sig) { (void)sig; _Exit(7); }
static void at_end(void) { printf("atexit ran\n"); }
int main(void) {
    atexit(at_end);
    signal(SIGABRT, on_abrt);
    printf("before abort\n");
    fflush(stdout);
    abort();
    printf("after abort\n");
    return 0;
}
