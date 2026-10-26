/* Memory model step 5b.4: a value live THROUGH a cold arm of the switch
 * into the join -- the dispatch loop's step counter, incremented at the
 * top of every iteration and read after the loop -- can be put in
 * memory across that arm only if it comes back as a value at the arm's
 * exit: a phi at the join with the region copy as its argument from the
 * arm, and the reads past the join (the back-edge phi, the code after
 * the loop) renamed to it.  Likewise a value from before the loop (the
 * table pointer) read in several arms: its join is a phi at the loop
 * header.  Ten loop-carried variables fit the callee-saved registers;
 * two arms hold eight temporaries across an indirect call and do not,
 * so the pressure is the arm's, and the cheapest value live across the
 * arm is the counter (or the table pointer), not a variable.  The harness
 * compiles this with HIR_REGION=1 HIR_RA_DEBUG=1 and expects a REGION
 * line with joins=1 or more. */
#include <stdio.h>

static int prog[64];
static unsigned tab[64];
#define EXPECT_X 1188626075u
#define EXPECT_Y 1124299105u

static unsigned mix10(unsigned a, unsigned b, unsigned c, unsigned d, unsigned e,
                      unsigned f, unsigned g, unsigned h, unsigned i, unsigned j) {
    return (a * 3) ^ (b << 1) ^ (c * 5) ^ (d >> 1) ^ (e * 7) ^ (f << 2) ^
           (g * 9) ^ (h >> 2) ^ (i * 11) ^ (j << 3);
}

static unsigned (*mixer)(unsigned, unsigned, unsigned, unsigned, unsigned,
                         unsigned, unsigned, unsigned, unsigned, unsigned) = mix10;

static unsigned run(int n, int seed, unsigned *t) {
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
    unsigned nstep = 0;    /* the counter: stepped at the top, read after the loop */
    unsigned acc = 0;
    int step;
    for (step = 0; step < n; step++) {
        int op;
        nstep++;
        op = prog[pc & 63];
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
        case 9: k ^= a; break;
        case 10: {
            unsigned t1 = a * 3, t2 = b * 5, t3 = c * 7, t4 = d * 9;
            unsigned t5 = e * 11, t6 = f * 13, t7 = g * 15, t8 = h * 17;
            acc += mixer(t1, t2, t3, t4, t5, t6, t7, t8, j, k) + t[op];
            acc ^= (t1 + t2) ^ (t3 + t4) ^ (t5 + t6) ^ (t7 + t8);
            break;
        }
        case 11: {
            unsigned u1 = k ^ 3, u2 = j ^ 5, u3 = h ^ 7, u4 = g ^ 9;
            unsigned u5 = f ^ 11, u6 = e ^ 13, u7 = d ^ 15, u8 = c ^ 17;
            acc += mixer(u1, u2, u3, u4, u5, u6, u7, u8, b, a) ^ t[pc & 63];
            acc += (u1 ^ u2) + (u3 ^ u4) + (u5 ^ u6) + (u7 ^ u8);
            break;
        }
        case 12: acc ^= t[(a + nstep) & 63]; break;
        case 13: t[(acc ^ pc) & 63] += b; break;
        default: acc += op; break;
        }
    }
    return acc + nstep * 17 + a + b + c + d + e + f + g + h + j + k;
}

int main(void) {
    int i;
    unsigned x, y;
    for (i = 0; i < 64; i++) { prog[i] = (i * 7 + 3) % 15; tab[i] = (unsigned)(i * 2654435761u); }
    x = run(1000, 3, tab);
    y = run(777, -5, tab);
    printf("%u %u\n", x, y);
    if (x != EXPECT_X || y != EXPECT_Y) return 1;
    return 0;
}
