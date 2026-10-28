// 6.8.2p2-3  mixed declarations and statements; inner block shadows; autos re-initialized on each entry
#include <stdio.h>
int main(void) {
    int x = 1, total = 0;
    total += x;
    int y = x + 1;
    {
        int x = 10;
        total += x + y;
        { int x = 100; total += x; }
        total += x;
    }
    for (int i = 0; i < 3; i++) {
        int k = 5;
        k += i;
        total += k;
        static int st;
        st++;
        if (i == 2) printf("st=%d\n", st);
    }
    total += x;
    printf("total=%d\n", total);
    return 0;
}
