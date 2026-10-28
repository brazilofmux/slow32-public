// 6.5.9p3-6  == and !=: null pointer constants, void * vs object pointer, function pointers
#include <stdio.h>
#include <stddef.h>
static void f(void) {}
int main(void) {
    int x; int *p = &x; void *v = p; int *n = NULL;
    void (*fp)(void) = f;
    printf("%d %d %d %d\n", p == v, n == 0, p != NULL, fp == f);
    printf("%d %d %d\n", 0.0 == -0.0, 3 == 3.0, (unsigned)-1 == -1);
    return 0;
}
