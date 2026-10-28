// 6.7.6.2p1-6  arrays: sized by constant expression, completed by initializer, multidimensional
#include <stdio.h>
int main(void) {
    int m[2][3] = { { 1, 2, 3 }, { 4, 5, 6 } };
    char s[] = "abcd";
    int n[] = { 1, 2, 3, 4, 5 };
    enum { K = 3 }; double d[K * 2];
    printf("%u %u %u %u %d\n", (unsigned)sizeof m, (unsigned)sizeof m[1], (unsigned)sizeof s,
           (unsigned)sizeof n, m[1][2]);
    printf("%u\n", (unsigned)sizeof d);
    return 0;
}
