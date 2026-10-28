// 6.4.4.3p2  enumeration constants have type int; usable in constant expressions
#include <stdio.h>
enum color { RED, GREEN = 5, BLUE, NEG = -3, AFTER };
int arr[BLUE];
int main(void) {
    switch (6) { case BLUE: printf("blue "); break; default: break; }
    printf("%d %d %d %d %d %d\n", RED, GREEN, BLUE, NEG, AFTER, (int)sizeof arr);
    printf("%d\n", (int)sizeof(RED));
    return 0;
}
