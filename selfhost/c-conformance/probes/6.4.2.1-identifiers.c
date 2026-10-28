// 6.4.2.1p2-p5  identifiers: underscores, digits, case-sensitive, at least 63 significant initial chars
#include <stdio.h>
int abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghij_1 = 1;
int abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghij_2 = 2;
int main(void) {
    int Var = 1, var = 2, _v9 = 3, v_9_ = 4;
    int a23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890x = 10;
    printf("%d %d\n", Var + var * 10 + _v9 * 100 + v_9_ * 1000,
           abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghij_1 * 10 +
           abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghij_2 +
           a23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890x);
    return 0;
}
