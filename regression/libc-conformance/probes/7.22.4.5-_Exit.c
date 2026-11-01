/* 7.22.4.5 _Exit: ends the program with the given status without calling atexit functions */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void bye(void) { printf("atexit must not run\n"); }
int main(void) {
    atexit(bye);
    printf("before\n");
    fflush(stdout);
    _Exit(5);
}
