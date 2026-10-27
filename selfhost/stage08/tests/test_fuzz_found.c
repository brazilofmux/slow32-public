/* Defects regression/fuzz/cfuzz.py found (2026-10-27), each reduced to
 * the shape that showed it.  Every function must compile and compute
 * what C says.
 *
 * ternary_switch: the SSA renamer looked up a variable for a phi the
 *   lowering made itself (a 64-bit ?:), read out of bounds, and faulted
 *   the compiler.
 * switch_then_break: the unroller's switch flag leaked to the switch's
 *   siblings, so the `break` after it counted as the switch's; the loop
 *   was unrolled and the compile failed ("break outside loop/switch").
 * after_inner: the peeler's loop flag leaked the same way; the break
 *   after the inner loop would have been peeled into the outer loop.
 * notnot: NOT(NOT(x)) became x; !!x is x != 0.
 * twophis: return pinning gave two phis of one join r1 (the check for
 *   it walked the block's instructions, where phis are not kept).
 * capped: ten unrolled copies needed eleven optimizer rounds; the last
 *   phi turned COPY was left with a reader, and a COPY on a phi list is
 *   never emitted.
 * ivret: return pinning put a loop's strength-reduced induction variable
 *   and the value returned from it in r1 together; the IV's increment
 *   still read it after the returned value was written. */
int ternary_switch(int i0) {
    unsigned long long v2 = 0;
    unsigned long long v3 = 0;
    v3 = (unsigned long long)(1u ? 1642325554u : (unsigned)i0);
    switch ((unsigned)v2 % 6u) {
    case 2:
        break;
    case 0:
        v2 |= (unsigned long long)0u;
        break;
    }
    return (int)(v3 & 0xffff) + (int)v2;
}

int switch_then_break(unsigned p) {
    int i;
    int t;
    t = 0;
    for (i = 0; i < 9; i++) {
        switch (p) {
        case 1:
            t = t + 10;
            break;
        }
        t = t + i;
        if (i >= 3) break;
    }
    return t * 10 + i;
}

int after_inner(int n) {
    int k;
    int i;
    int j;
    int t;
    t = 0;
    for (k = 0; k < 2; k++) {
        for (i = 0; i < 5; i++) {
            for (j = 0; j < n; j++) t = t + j;
            if (t > 20) break;
            if (i != 4) t = t + 3;
        }
        t = t + 100 * i;
    }
    return t;
}

unsigned notnot(unsigned p) {
    unsigned long long v1;
    v1 = !p;
    v1 = !(unsigned)v1;
    return (unsigned)v1 + 10u * (unsigned)!!p;
}

unsigned twophis(unsigned p0) {
    unsigned v0 = 0xffffffffu;
    unsigned long long v1 = 0;
    unsigned v2 = 0;
    switch (0u % 6u) {
    case 3:
        break;
    case 2:
    case 5:
        v1 = 0;
        v0 = 0;
        break;
    }
    v2 ^= v0;
    return p0 ? (unsigned)v1 : v2;
}

unsigned capped(unsigned p1) {
    unsigned v1 = 8;
    int i0;
    for (i0 = 11; i0 >= 2; i0 -= 1) v1 ^= ((v1 ? 1u : p1) - (unsigned)i0);
    return v1;
}

unsigned ivret(unsigned p0) {
    unsigned v4 = 0;
    int i0;
    int i1;
    for (i0 = 0; i0 < 3; i0 += 1) {
        for (i1 = 3; i1 < 8; i1 += 1) {
            v4 = ((unsigned)i0 << 13) >> 11;
        }
    }
    return v4 + p0;
}

int main(void) {
    if (ternary_switch(3) != 58930) return 1;
    if (switch_then_break(1) != 463 || switch_then_break(0) != 63) return 2;
    if (after_inner(3) != 324 || after_inner(0) != 512) return 3;
    if (notnot(38) != 11 || notnot(0) != 0) return 4;
    if (twophis(1) != 0 || twophis(0) != 0xffffffffu) return 5;
    if (capped(3302909072u) != 9 || capped(5) != 9) return 6;
    if (ivret(0) != 8) return 7;
    return 0;
}
