// 6.10.3p7,p9  object-like macros, rescanning of the replacement, outer blanks dropped
#include <stdio.h>
#define N    10   
#define TWICE_N (N + N)
#define TABLE_SIZE TWICE_N
int arr[TABLE_SIZE];
int main(void) {
    printf("%d %d\n", (int)(sizeof arr / sizeof arr[0]), TABLE_SIZE);
    return 0;
}
