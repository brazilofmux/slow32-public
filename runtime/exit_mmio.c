#include <stdlib.h>
#include <signal.h>

#include "mmio_ring.h"

extern void yield(void);
extern void __cxa_finalize(void *dso_handle);

/* What stdio wants done when the program ends: stdio.c puts its routine
 * here the first time a stream holds anything (send what the streams
 * still hold).  A pointer, not a call, so a program that never touches
 * stdio does not link it. */
void (*__stdio_exit_hook)(void);

static void halt_with(int status) __attribute__((noreturn));
static void halt_with(int status) {
    unsigned int req_head = S32_MMIO_REQ_HEAD;
    unsigned int req_tail = S32_MMIO_REQ_TAIL;
    volatile unsigned int *req_ring = S32_MMIO_REQ_RING;

    while (((req_head + 1u) % S32_MMIO_RING_ENTRIES) == req_tail) {
        yield();
        req_tail = S32_MMIO_REQ_TAIL;
    }

    unsigned int idx = req_head * S32_MMIO_DESC_WORDS;
    req_ring[idx + 0] = S32_MMIO_OP_EXIT;
    req_ring[idx + 1] = 0;
    req_ring[idx + 2] = 0;
    req_ring[idx + 3] = (unsigned int)status;

    S32_MMIO_REQ_HEAD = (req_head + 1u) % S32_MMIO_RING_ENTRIES;

    while (1) yield();
}

static void flush_stdio(void) {
    if (__stdio_exit_hook) {
        void (*hook)(void) = __stdio_exit_hook;
        __stdio_exit_hook = 0;
        hook();
    }
}

/* atexit functions and static destructors (cxxabi.c keeps both in one
 * list), then what the streams hold, then the end */
void exit(int status) {
    __cxa_finalize(0);
    flush_stdio();
    halt_with(status);
}

/* the end, here and now: no atexit functions, nothing flushed */
void _exit(int status) {
    halt_with(status);
}

void _Exit(int status) {
    halt_with(status);
}

/* a signal's default action (signal.c): what was written is sent -- the
 * standard leaves that to the library -- but this is not exit, and no
 * atexit function runs */
void __s32_killed(int sig) {
    flush_stdio();
    halt_with(128 + sig);
}

/* abort raises SIGABRT; a handler that returns does not save the program */
void abort(void) {
    raise(SIGABRT);
    signal(SIGABRT, SIG_DFL);
    raise(SIGABRT);
    halt_with(128 + SIGABRT);
}
