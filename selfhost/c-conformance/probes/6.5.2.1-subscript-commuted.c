// 6.5.2.1p2  E1[E2] is identical to (*((E1)+(E2))), so the integer may come first: 2[a]
#include <stdio.h>
int main(void) {
    int a[4] = {10, 11, 12, 13};
    int m[3][4];
    for (int i = 0; i < 12; i++) m[i / 4][i % 4] = i;
    printf("%d %d\n", 2[a], 2[m][1]);
    return 0;
}
