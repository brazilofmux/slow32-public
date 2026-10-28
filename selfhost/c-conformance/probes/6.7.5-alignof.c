// 6.5.3.4p3  _Alignof yields the alignment requirement of a type
#include <stdio.h>
struct s { char c; double d; };
int main(void) {
    printf("%u %u %u %u %u\n", (unsigned)_Alignof(char), (unsigned)_Alignof(int),
           (unsigned)_Alignof(long long), (unsigned)_Alignof(double), (unsigned)_Alignof(struct s));
    return 0;
}
