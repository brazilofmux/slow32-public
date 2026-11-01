/* 7.22.2.2 srand: rand before any srand behaves as after srand(1), and the same seed repeats the sequence */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(void) {
    int a[50], b[50], c[50], i, same1 = 1, same42 = 1, differs = 0;
    for (i = 0; i < 50; i++) a[i] = rand();
    srand(1);
    for (i = 0; i < 50; i++) if (rand() != a[i]) same1 = 0;
    srand(42);
    for (i = 0; i < 50; i++) b[i] = rand();
    srand(7);
    for (i = 0; i < 50; i++) c[i] = rand();
    srand(42);
    for (i = 0; i < 50; i++) if (rand() != b[i]) same42 = 0;
    for (i = 0; i < 50; i++) if (b[i] != c[i]) differs = 1;
    printf("default seed is 1: %d\nreseed repeats: %d\nseeds differ: %d\n", same1, same42, differs);
    return 0;
}
