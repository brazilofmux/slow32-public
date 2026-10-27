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
 *   still read it after the returned value was written.
 * pinconst: a ?: arm folded to a large constant left the constant in the
 *   arm's block; pinned as a loop constant it was loaded in the inner
 *   loop, after the outer loop's hoisted subtraction had read it.
 * retbody: a counted loop whose body always returns was unrolled copy
 *   after copy past the return ("instructions after a block's
 *   terminator").
 * twovals: return pinning put v0 and v1's next value in r1 together;
 *   they were live at once (the allocator said so; nothing asked it). */
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

unsigned pg3 = 7u;
unsigned pg0 = 11u;

unsigned pinconst(unsigned p0) {
    int i1 = 0;
    int i2;
    int i4;
    unsigned n = 0;
    for (i2 = 1; i2 < 7; i2 += 2) {
        for (i4 = 14; i4 >= 0; i4 -= 1) {
            if (((unsigned)i2 - (((unsigned)i1 <= 4294967295u) ? 4096u : pg0)) > pg3) return 100u + (unsigned)i2;
            n = n + p0;
        }
    }
    return n;
}

unsigned rb_g;

unsigned retbody(unsigned p1) {
    int i0;
    for (i0 = 3; i0 < 15; i0 += 1) {
        rb_g -= p1;
        return 100u + rb_g;
    }
    return 0u;
}

unsigned twovals(void) {
    unsigned v0 = 0;
    unsigned v1 = 0;
    int i0;
    int i1;
    for (i0 = 1; i0 < 8; i0 += 2) {
        v0 = v1 - 16u;
        for (i1 = 2; i1 < 3; i1 += 1) {
            v1 = (unsigned)!(unsigned)i0;
        }
    }
    return v0;
}

int main(void) {
    if (ternary_switch(3) != 58930) return 1;
    if (switch_then_break(1) != 463 || switch_then_break(0) != 63) return 2;
    if (after_inner(3) != 324 || after_inner(0) != 512) return 3;
    if (notnot(38) != 11 || notnot(0) != 0) return 4;
    if (twophis(1) != 0 || twophis(0) != 0xffffffffu) return 5;
    if (capped(3302909072u) != 9 || capped(5) != 9) return 6;
    if (ivret(0) != 8) return 7;
    if (pinconst(1) != 101) return 8;
    if (retbody(1) != 99u || rb_g != 0xffffffffu) return 9;
    if (twovals() != 0xfffffff0u) return 10;
    return 0;
}
