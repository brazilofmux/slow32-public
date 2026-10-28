// 6.10.3p12, 6.10.3.1p2  variadic macros: forwarding __VA_ARGS__, stringizing it, a fixed parameter before it
#include <stdio.h>
#define say(...) printf(__VA_ARGS__)
#define quote(...) puts(#__VA_ARGS__)
#define check(cond, ...) ((cond) ? puts(#cond) : printf(__VA_ARGS__))
int main(void) {
    int lo = 2, hi = 9;
    say("plain\n");
    say("lo=%d hi=%d\n", lo, hi);
    quote(red,  green , blue  and the rest);
    check(lo < hi, "never %d\n", lo);
    check(lo > hi, "lo %d is not above hi %d\n", lo, hi);
    return 0;
}
