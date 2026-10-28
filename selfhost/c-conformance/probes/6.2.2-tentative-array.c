// 6.9.2p2 (via 6.2.2p5)  a tentative array of unknown size with no later definition becomes one element, zeroed
#include <stdio.h>
int arr[];
int main(void) {
    arr[0] += 5;
    printf("%d %d\n", arr[0], (int)sizeof arr[0]);
    return 0;
}
