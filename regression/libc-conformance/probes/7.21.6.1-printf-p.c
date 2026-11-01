/* 7.21.6.1 fprintf: %p prints some non-empty implementation-defined text, the same for equal pointers, different for different ones, and a field width pads it with spaces */
#include <stdio.h>
#include <string.h>

int main(void) {
    int a[2];
    char x[64], y[64], z[64], w[64], l[64];
    int n = snprintf(x, sizeof x, "%p", (void *)&a[0]);
    snprintf(y, sizeof y, "%p", (void *)&a[0]);
    snprintf(z, sizeof z, "%p", (void *)&a[1]);
    printf("%d %d %d\n", n > 0, strcmp(x, y) == 0, strcmp(x, z) != 0);
    snprintf(w, sizeof w, "%40p", (void *)&a[0]);
    snprintf(l, sizeof l, "%-40p", (void *)&a[0]);
    size_t k = strlen(x);
    int right = strlen(w) == 40 && strcmp(w + 40 - k, x) == 0 && strspn(w, " ") == 40 - k;
    int left = strlen(l) == 40 && strncmp(l, x, k) == 0 && strspn(l + k, " ") == 40 - k;
    printf("%d %d\n", k < 40 && right, k < 40 && left);
    return 0;
}
