/* 7.19p3: NULL is a null pointer constant, usable for object and function pointers */
#include <stddef.h>
#include <stdio.h>

int main(void) {
    char *p = NULL;
    int (*fp)(void) = NULL;
    void *v = NULL;
    printf("%d %d %d %d %d\n", p == 0, fp == 0, v == 0, (void *)NULL == (void *)0, !NULL);
    return 0;
}
