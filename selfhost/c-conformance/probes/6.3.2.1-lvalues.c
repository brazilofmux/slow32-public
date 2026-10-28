// 6.3.2.1p3  arrays decay to pointer except under sizeof, unary & and string-literal init
#include <stdio.h>
int main(void) {
    int arr[10];
    char s[] = "hello";
    int *p = arr;
    arr[2] = 5;
    printf("%d %d %d %d %d %d\n", (int)sizeof arr, (int)sizeof(arr + 0), (int)sizeof s,
           (int)sizeof "abc", p[2], (int)(&arr[3] - arr));
    return 0;
}
