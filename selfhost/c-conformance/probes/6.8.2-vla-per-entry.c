// 6.8.2p3  a VLA in a block is (re)created with its size evaluated on each entry
// optional: __STDC_NO_VLA__
#include <stdio.h>
int main(void) {
    int sum = 0;
    for (int n = 1; n <= 5; n++) {
        int a[n];
        for (int i = 0; i < n; i++) a[i] = i * n;
        for (int i = 0; i < n; i++) sum += a[i];
        sum += (int)sizeof a;
    }
    printf("%d\n", sum);
    return 0;
}
