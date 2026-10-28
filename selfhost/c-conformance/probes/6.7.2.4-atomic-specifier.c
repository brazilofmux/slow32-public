// 6.7.2.4p1-4  _Atomic(type) specifier: arithmetic and compound assignment on atomic objects
#include <stdio.h>
_Atomic(int) counter = 5;
_Atomic(long long) big;
int main(void) {
    counter += 3; counter++; ++counter;
    big = 1LL << 40; big += 1;
    _Atomic(int) local = counter * 2;
    printf("%d %lld %d %u\n", (int)counter, (long long)big, (int)local, (unsigned)sizeof(_Atomic(int)));
    return 0;
}
