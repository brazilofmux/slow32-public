/* 7.22.4.2 atexit, 7.22.4.4 exit: registered functions run in reverse order of registration, once each */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void f1(void) { printf("f1\n"); }
static void f2(void) { printf("f2\n"); }
static void f3(void) { printf("f3\n"); }
int main(void) {
    printf("%d", atexit(f1) == 0);
    printf("%d", atexit(f2) == 0);
    printf("%d", atexit(f3) == 0);
    printf("%d\n", atexit(f1) == 0);
    exit(0);
}
