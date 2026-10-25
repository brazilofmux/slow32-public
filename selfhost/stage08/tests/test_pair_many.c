/* An 8-byte local -- long long or double -- is split into two word
 * allocas so mem2reg can promote both halves.  The splitter kept the
 * hi-half address nodes in an array of eight and refused any local
 * with more: sqlite3GetToken's `i64 i`, assigned in sixteen arms,
 * stayed in the frame, as did sqlite3VdbeExec's nVmStep (selfhost
 * ISSUES-98).  (A `++` or `+=` on one still loads its old value typed
 * as the whole 8 bytes, which the splitter refuses; typing it as a word
 * promoted every i64 in sqlite3VdbeExec and doubled its spills, so that
 * waits for the allocator -- docs/plans/memory-model.md.)  The harness
 * checks that many() touches no frame slot. */
#include <stdio.h>
typedef long long i64;

static i64 many(int c, int k) {
    i64 i = 1;
    switch (c) {
    case 0: i = k; break;
    case 1: i = i + k; break;
    case 2: i = i * 3; break;
    case 3: i = (i64)k << 33; break;
    case 4: i = -k; break;
    case 5: i = i + 7; break;
    case 6: i = k * 2; break;
    case 7: i = i - k; break;
    case 8: i = (i64)k * 1000000007LL; break;
    case 9: i = i + 11; break;
    case 10: i = k | 5; break;
    case 11: i = i + 13; break;
    default: i = 99; break;
    }
    i = i + 1;
    return i;
}

static double dmany(int c, double d) {
    double x = 0.5;
    switch (c) {
    case 0: x = d; break;
    case 1: x = x + d; break;
    case 2: x = x * 3.0; break;
    case 3: x = d - 1.0; break;
    case 4: x = -d; break;
    case 5: x = x + 7.0; break;
    case 6: x = d * 2.0; break;
    case 7: x = x - d; break;
    case 8: x = d / 4.0; break;
    case 9: x = x + 11.0; break;
    default: x = 99.0; break;
    }
    return x + 1.0;
}

int main(void) {
    if (many(0, 5) != 6) return 1;
    if (many(1, 5) != 7) return 2;
    if (many(3, 3) != 25769803777LL) return 3;
    if (many(4, 5) != -4) return 4;
    if (many(8, 3) != 3000000022LL) return 5;
    if (many(11, 0) != 15) return 6;
    if (many(40, 0) != 100) return 7;
    if (dmany(0, 2.25) != 3.25) return 8;
    if (dmany(8, 10.0) != 3.5) return 9;
    if (dmany(40, 0.0) != 100.0) return 10;
    printf("pair many ok\n");
    return 0;
}
