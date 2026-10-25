/* A loop-carried variable assigned in one arm of a big switch and read
 * in another: at the loop header it is phi(init, M), and M is the join's
 * phi with the header value on every pass-through edge.  Under pressure
 * the header phi is spilled while M, live only across the join, keeps a
 * register, and the back edge stores M into the slot on every iteration
 * -- four such stores per opcode in sqlite3VdbeExec's dispatch loop.
 * M's only use is the header phi, so it takes the same slot: the copy
 * is a no-op and the assigning arms store straight into it.  The harness
 * compiles this with HIR_RA_DEBUG and expects a PHIWEB line. */
#include <stdio.h>

static int prog[64];
#define EXPECT_X 303882874LL
#define EXPECT_Y 21478138936LL

static long long run(int n, int seed) {
    int pc = 0;
    unsigned a = seed + 0;
    unsigned b = seed + 1;
    unsigned c = seed + 2;
    unsigned d = seed + 3;
    unsigned e = seed + 4;
    unsigned f = seed + 5;
    unsigned g = seed + 6;
    unsigned h = seed + 7;
    unsigned j = seed + 8;
    unsigned k = seed + 9;
    unsigned l = seed + 10;
    unsigned m = seed + 11;
    unsigned o = seed + 12;
    unsigned q = seed + 13;
    unsigned r = seed + 14;
    unsigned s = seed + 15;
    unsigned t = seed + 16;
    unsigned u = seed + 17;
    unsigned v = seed + 18;
    unsigned w = seed + 19;
    unsigned x = seed + 20;
    unsigned y = seed + 21;
    unsigned z = seed + 22;
    unsigned aa = seed + 23;
    unsigned ab = seed + 24;
    unsigned ac = seed + 25;
    unsigned ad = seed + 26;
    unsigned ae = seed + 27;
    unsigned af = seed + 28;
    unsigned ag = seed + 29;
    unsigned ah = seed + 30;
    unsigned ai = seed + 31;
    unsigned aj = seed + 32;
    unsigned hold = 0;     /* assigned in one arm, read in another */
    long long acc = 0;
    int step;
    for (step = 0; step < n; step++) {
        int op = prog[pc & 63];
        pc++;
        switch (op) {
        case 0: a += b; break;
        case 1: b ^= c; break;
        case 2: c += d; break;
        case 3: d ^= e; break;
        case 4: e += f; break;
        case 5: f ^= g; break;
        case 6: g += h; break;
        case 7: h ^= j; break;
        case 8: j += k; break;
        case 9: k ^= l; break;
        case 10: l += m; break;
        case 11: m ^= o; break;
        case 12: o += q; break;
        case 13: q ^= r; break;
        case 14: r += s; break;
        case 15: s ^= t; break;
        case 16: t += u; break;
        case 17: u ^= v; break;
        case 18: v += w; break;
        case 19: w ^= x; break;
        case 20: x += y; break;
        case 21: y ^= z; break;
        case 22: z += aa; break;
        case 23: aa ^= ab; break;
        case 24: ab += ac; break;
        case 25: ac ^= ad; break;
        case 26: ad += ae; break;
        case 27: ae ^= af; break;
        case 28: af += ag; break;
        case 29: ag ^= ah; break;
        case 30: ah += ai; break;
        case 31: ai ^= aj; break;
        case 32: aj += a; break;
        case 33: hold = a + b + c + d + e + f + g + h; break;
        case 34: acc += hold; break;
        case 35: acc += j + k + l + m + o + q + r + s + t + u + v + w + x + y + z + aa + ab + ac + ad + ae + af + ag + ah + ai + aj; break;
        default: acc += op; break;
        }
    }
    return acc + a + b + c + d + e + f + g + h + j + k + l + m + o + q + r + s + t + u + v + w + x + y + z + aa + ab + ac + ad + ae + af + ag + ah + ai + aj;
}

int main(void) {
    int i;
    long long x, y;
    for (i = 0; i < 64; i++) prog[i] = (i * 7 + 3) % 37;
    x = run(1000, 3);
    y = run(777, -5);
    printf("%lld %lld\n", x, y);
    if (x != EXPECT_X || y != EXPECT_Y) return 1;
    return 0;
}
