/* Freestanding thread_local destructors link through __cxa_thread_atexit
 * (the Itanium name); libc previously exported only _impl. */
#include <stdio.h>

int __cxa_thread_atexit(void (*destructor)(void *), void *obj, void *dso_symbol);

static void dtor(void *p) {
    printf("tls dtor %d\n", *(int *)p);
}

int main(void) {
    static int x = 7;
    if (__cxa_thread_atexit(dtor, &x, 0) != 0) {
        printf("register failed\n");
        return 1;
    }
    printf("registered\n");
    return 0;
}
