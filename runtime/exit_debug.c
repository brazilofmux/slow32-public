#include <stdlib.h>
#include <signal.h>

extern void halt(void);
extern void __cxa_finalize(void *dso_handle);

static void halt_with(int status) __attribute__((noreturn));
static void halt_with(int status) {
    /* Every emulator reports r1 at halt as the process exit status
     * (slow32.c: `int exit_code = cpu.regs[1]`), so put the status
     * there before halting.  Calling halt() as a function left r1 as
     * whatever the last call returned, and every DEBUG-libc program
     * exited 0 no matter what main returned. */
    __asm__ __volatile__("add r1, %0, r0\n\thalt r0, r0, 0" : : "r"(status));
    while (1) {
    }
}

void exit(int status) {
    __cxa_finalize(0);
    halt_with(status);
}

/* the end, here and now: no atexit functions */
void _exit(int status) {
    halt_with(status);
}

void _Exit(int status) {
    halt_with(status);
}

/* a signal's default action (signal.c); nothing is buffered in this library */
void __s32_killed(int sig) {
    halt_with(128 + sig);
}

/* abort raises SIGABRT; a handler that returns does not save the program */
void abort(void) {
    raise(SIGABRT);
    signal(SIGABRT, SIG_DFL);
    raise(SIGABRT);
    halt_with(128 + SIGABRT);
}
