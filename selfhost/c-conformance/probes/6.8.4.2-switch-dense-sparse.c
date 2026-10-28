// 6.8.4.2p5  dense (table) and sparse switches, negative and boundary case values
#include <stdio.h>
#include <limits.h>
static int dense(int x) {
    switch (x) {
    case 0: return 10; case 1: return 11; case 2: return 12; case 3: return 13;
    case 4: return 14; case 5: return 15; case 6: return 16; case 8: return 18;
    case 9: return 19; case 10: return 20; default: return -1;
    }
}
static int sparse(int x) {
    switch (x) {
    case INT_MIN: return 1; case -1000: return 2; case 0: return 3;
    case 77777: return 4; case INT_MAX: return 5; case 1 << 20: return 6;
    default: return 0;
    }
}
static int unsd(unsigned x) {
    switch (x) { case 0: return 1; case 1: return 2; case 2: return 3; case 3: return 4; case 4: return 5; case 0x80000000u: return 6; default: return 0; }
}
int main(void) {
    int s = 0;
    for (int i = -3; i < 14; i++) s = s * 3 + dense(i);
    int v[] = { INT_MIN, -1000, 0, 77777, INT_MAX, 1 << 20, 5, INT_MIN + 1, -1 };
    for (int i = 0; i < 9; i++) printf("%d", sparse(v[i]));
    printf(" %d %d%d%d\n", s, unsd(0x80000000u), unsd(4), unsd(0xFFFFFFFFu));
    return 0;
}
