/* HOSTLEG -- _Exit ends the program there: nothing atexit registered
 * runs, and nothing after the call. */
#include <stdio.h>
#include <stdlib.h>

static void never(void) { printf("atexit: MUST NOT RUN at _Exit\n"); }

int main(void) {
    atexit(never);
    printf("calling _Exit\n");
    fflush(stdout);
    _Exit(0);
    printf("NOT REACHED\n");
    return 1;
}
