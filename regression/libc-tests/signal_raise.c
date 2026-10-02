/* HOSTLEG -- signal and raise: what signal returns, that raise runs the
 * handler with the signal's number before it returns, that an ignored
 * signal is ignored, and that abort ends the program through SIGABRT's
 * handler -- having run it. */
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

static volatile sig_atomic_t got;
static int calls;

static void on_signal(int sig) {
    got = sig;
    calls++;
}

static void other(int sig) {
    got = -sig;
    calls++;
}

static void on_abort(int sig) {
    printf("SIGABRT handler runs (%d)\n", sig == SIGABRT);
    fflush(stdout);
}

static void bye(void) {
    printf("atexit handler: MUST NOT RUN after abort\n");
}

int main(void) {
    void (*prev)(int);
    int r;

    prev = signal(SIGINT, on_signal);
    printf("first signal(): previous was %s\n", prev == SIG_DFL ? "SIG_DFL" : "something else");
    prev = signal(SIGINT, other);
    printf("second: previous was %s\n", prev == on_signal ? "the first handler" : "something else");
    r = raise(SIGINT);
    printf("raise returned %d, handler saw %d, calls %d\n", r, (int)got, calls);

    signal(SIGTERM, on_signal);
    r = raise(SIGTERM);
    printf("raise(SIGTERM) returned %d, handler saw %d (SIGTERM %d), calls %d\n", r, (int)got, got == SIGTERM, calls);
    signal(SIGTERM, on_signal);
    signal(SIGINT, on_signal);
    raise(SIGINT);
    printf("each signal has its own handler: saw SIGINT %d, calls %d\n", got == SIGINT, calls);

    prev = signal(SIGTERM, SIG_IGN);
    r = raise(SIGTERM);
    printf("ignored: raise returned %d, calls %d\n", r, calls);
    prev = signal(SIGTERM, SIG_DFL);
    printf("previous was %s\n", prev == SIG_IGN ? "SIG_IGN" : "something else");

    signal(SIGFPE, on_signal);
    signal(SIGILL, on_signal);
    signal(SIGSEGV, on_signal);
    raise(SIGFPE);  printf("SIGFPE %d", got == SIGFPE);
    signal(SIGILL, on_signal);
    raise(SIGILL);  printf(" SIGILL %d", got == SIGILL);
    signal(SIGSEGV, on_signal);
    raise(SIGSEGV); printf(" SIGSEGV %d, calls %d\n", got == SIGSEGV, calls);

    printf("signal(no such signal): %s\n", signal(12345, on_signal) == SIG_ERR ? "SIG_ERR" : "accepted");
    printf("raise(no such signal): %s\n", raise(12345) != 0 ? "nonzero" : "zero");

    atexit(bye);
    signal(SIGABRT, on_abort);
    printf("calling abort\n");
    fflush(stdout);
    abort();
    printf("NOT REACHED\n");
    return 0;
}
