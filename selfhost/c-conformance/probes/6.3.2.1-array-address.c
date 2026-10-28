// 6.3.2.1p3  &array is not converted: it yields a pointer to the whole array
#include <stdio.h>
int main(void) {
    int arr[10];
    int (*pa)[10] = &arr;
    (*pa)[1] = 7;
    printf("%d %d %d\n", (int)sizeof *pa, arr[1], (int)((char *)(pa + 1) - (char *)arr));
    return 0;
}
