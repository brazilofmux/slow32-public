/* 7.22.4.4 exit, 5.1.2.2.3: returning from main is exit with the returned value */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void bye(void) { printf("handler\n"); }
int main(void) {
    atexit(bye);
    printf("main\n");
    return 42;
}
