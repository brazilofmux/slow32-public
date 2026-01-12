#include <stdlib.h>

extern void halt(void);
extern void __cxa_finalize(void *dso_handle);

void exit(int status) {
    (void)status;
    __cxa_finalize(0);
    halt();
    while (1) {
    }
}

void abort(void) {
    exit(EXIT_FAILURE);
}
