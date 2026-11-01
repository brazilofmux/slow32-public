/* 7.21.6.2 fscanf: returns EOF on input failure before the first conversion, else the count of assignments */
#include <stdio.h>

int main(void) {
    int a = -1, b = -1;
    printf("%d\n", sscanf("", "%d", &a) == EOF);
    printf("%d\n", sscanf("   ", "%d", &a) == EOF);
    printf("%d\n", sscanf("abc", "%d", &a));
    printf("%d %d\n", sscanf("5 abc", "%d %d", &a, &b), a);
    printf("%d\n", sscanf("5", "%d %d", &a, &b));
    printf("%d\n", sscanf("x", "y%d", &a));
    printf("%d\n", sscanf("", "y%d", &a) == EOF);
    printf("%d\n", sscanf("", "") );
    printf("%d\n", sscanf("abc", "abc"));
    printf("%d\n", sscanf("", "%*d") == EOF);
    printf("%d\n", sscanf("-", "%d", &a));
    printf("%d\n", sscanf("+", "%d", &a));
    return 0;
}
