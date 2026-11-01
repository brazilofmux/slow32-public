/* 7.22.3.5 realloc: a null pointer acts like malloc; contents survive to the lesser of the two sizes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(void) {
    unsigned char *p = realloc(NULL, 100), *q;
    int i, ok = 1;
    if (!p) return 2;
    for (i = 0; i < 100; i++) p[i] = (unsigned char)(i * 7 + 1);
    q = realloc(p, 10);                         /* shrink */
    if (!q) return 3;
    for (i = 0; i < 10; i++) if (q[i] != (unsigned char)(i * 7 + 1)) ok = 0;
    printf("shrink keeps: %d\n", ok);
    p = realloc(q, 100000);                     /* grow, likely moves */
    if (!p) return 4;
    for (i = 0; i < 10; i++) if (p[i] != (unsigned char)(i * 7 + 1)) ok = 0;
    memset(p + 10, 0x33, 100000 - 10);
    q = realloc(p, 50);
    for (i = 0; i < 10; i++) if (q[i] != (unsigned char)(i * 7 + 1)) ok = 0;
    for (i = 10; i < 50; i++) if (q[i] != 0x33) ok = 0;
    printf("grow and shrink keep: %d\n", ok);
    free(q);
    return 0;
}
