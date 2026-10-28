// 6.5.3.4p3  _Alignof yields the alignment of a type name (char, double, struct, array)
#include <stdio.h>
struct S { char c; double d; };
int main(void) {
    printf("%d %d %d %d\n", (int)_Alignof(char), (int)_Alignof(double), (int)_Alignof(struct S), (int)_Alignof(int[4]));
    return 0;
}
