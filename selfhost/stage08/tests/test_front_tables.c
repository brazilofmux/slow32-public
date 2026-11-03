/* Past the parser's old fixed tables, all of which grow now (tables.h):
 * 5000 typedefs (4096), 17000 enum constants (16384), 33000 globals
 * (32768), 70000 distinct string literals (65536, whose writers dropped
 * the rest without a word).  Made by pasting. */
#include <string.h>
#define X10(M, n) M(n##0) M(n##1) M(n##2) M(n##3) M(n##4) M(n##5) M(n##6) M(n##7) M(n##8) M(n##9)
#define X100(M, n) X10(M, n##0) X10(M, n##1) X10(M, n##2) X10(M, n##3) X10(M, n##4) X10(M, n##5) X10(M, n##6) X10(M, n##7) X10(M, n##8) X10(M, n##9)
#define X1000(M, n) X100(M, n##0) X100(M, n##1) X100(M, n##2) X100(M, n##3) X100(M, n##4) X100(M, n##5) X100(M, n##6) X100(M, n##7) X100(M, n##8) X100(M, n##9)
#define X10000(M, n) X1000(M, n##0) X1000(M, n##1) X1000(M, n##2) X1000(M, n##3) X1000(M, n##4) X1000(M, n##5) X1000(M, n##6) X1000(M, n##7) X1000(M, n##8) X1000(M, n##9)

#define TD(n) typedef int t##n;
X1000(TD, a) X1000(TD, b) X1000(TD, c) X1000(TD, d) X1000(TD, e)
#define EC(n) e##n,
enum many { X10000(EC, a) X1000(EC, b0) X1000(EC, b1) X1000(EC, b2) X1000(EC, b3) X1000(EC, b4) X1000(EC, b5) X1000(EC, b6) e_end };

/* 33000 globals declared (the parser's table), three defined: the
 * stage07 assembler the suite uses holds 32768 symbols */
#define GV(n) extern int g##n;
X10000(GV, a) X10000(GV, b) X10000(GV, c) X1000(GV, d0) X1000(GV, d1) X1000(GV, d2)
int ga0000 = 1, gc9999 = 1, gd2999 = 1;

/* 70000 distinct literals, adjacent: each one is in the literal table,
 * and only sizeof sees their concatenation, so none is emitted (the
 * stage07 assembler and linker the suite uses would not hold them) */
#define SL(n) #n
#define ALL_LITS X10000(SL, a) X10000(SL, b) X10000(SL, c) X10000(SL, d) X10000(SL, e) X10000(SL, f) X10000(SL, g)
static const char few[] = X10(SL, x) X10(SL, y);

int main(void) {
    te999 x = 3;
    if (e_end != 17000) return 1;
    if (ga0000 + gc9999 + gd2999 != 3) return 2;
    if (sizeof(ALL_LITS) != 70000 * 5 + 1) return 3;
    if (strcmp(few, "x0x1x2x3x4x5x6x7x8x9y0y1y2y3y4y5y6y7y8y9") != 0) return 4;
    return x == 3 ? 0 : 5;
}
