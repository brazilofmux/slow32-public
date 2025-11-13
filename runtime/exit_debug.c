#include <stdlib.h>

extern void halt(void);

void exit(int status) {
    (void)status;
    halt();
    while (1) {
    }
}

void abort(void) {
    exit(EXIT_FAILURE);
}
