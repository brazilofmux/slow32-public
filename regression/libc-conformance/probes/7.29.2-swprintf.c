/* 7.29.2.3 swprintf: wide formatted output into an array, counted in wide characters; then narrowed for printing */
#include <stdio.h>
#include <wchar.h>

int main(void) {
    wchar_t w[64];
    int n, i;
    n = swprintf(w, 64, L"%d|%5.2f|%ls|%c|%x", 42, 3.14159, L"wide", 'c', 255);
    printf("count %d: ", n);
    for (i = 0; i < n; i++) putchar((int)w[i]);
    printf("\n");
    n = swprintf(w, 4, L"%s", "too long");
    printf("does not fit: negative %d\n", n < 0);
    return 0;
}
