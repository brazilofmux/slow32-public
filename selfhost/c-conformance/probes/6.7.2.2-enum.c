// 6.7.2.2p3  enumerator values: implicit increment, explicit, negative, repeats, int type
#include <stdio.h>
enum e { A, B, C = 10, D, E = -3, F, G = 1, H };
enum { LAST = H + 100 };
int main(void) {
    enum e v = D;
    printf("%d %d %d %d %d %d %d %d %d\n", A, B, C, D, E, F, G, H, LAST);
    printf("%d %u %u\n", v, (unsigned)sizeof(enum e), (unsigned)sizeof(A));
    int arr[C]; printf("%u\n", (unsigned)(sizeof arr / sizeof arr[0]));
    switch (v) { case D: puts("D"); break; default: puts("?"); }
    return 0;
}
