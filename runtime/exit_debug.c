#include <stdlib.h>

extern void halt(void);
extern void __cxa_finalize(void *dso_handle);

void exit(int status) {
    __cxa_finalize(0);
    /* Every emulator reports r1 at halt as the process exit status
     * (slow32.c: `int exit_code = cpu.regs[1]`), so put the status
     * there before halting.  Calling halt() as a function left r1 as
     * whatever the last call returned, and every DEBUG-libc program
     * exited 0 no matter what main returned. */
    __asm__ __volatile__("add r1, %0, r0\n\thalt r0, r0, 0" : : "r"(status));
    while (1) {
    }
}

void abort(void) {
    exit(EXIT_FAILURE);
}
