/* Integer <-> floating-point conversions, every width and signedness
 * (tools/dbt/ISSUES.md, DBT-20).
 *
 * The shared lowering took the signed conversions for unsigned sources
 * and destinations, and moved the bits across unconverted between long
 * long and float, on SLOW-32 and both 64-bit hosts: (float)(long long)1
 * was 1.4e-45, (double)3000000000u was -1294967296.0.  Assignment
 * converted nothing across the int/fp line (`u = d` stored the double's
 * low word), nor did float arithmetic with an integer operand or
 * `k += 1.5`.  That is how the self-hosted dbt-x64 ran FCVT.S.L and
 * FCVT.D.WU wrong.  On the way: SLOW-32's inline fneg.d (and fsqrt.d,
 * fcvt.d.l) wrote its operand's register pair in place, so a negated
 * loop invariant flipped sign every trip; cc-x64 read an unspilled f32
 * constant as 0; and cc-x64 pointed every function pointer in a static
 * initializer at the start of .data.
 *
 * The expected bits are gcc/clang's.  Out-of-range conversions are
 * undefined and not tested.  Returns the first failing check, 0 if all
 * pass.  Also in the cross trees' diff-test corpus (d38_fp_convert.c). */

typedef unsigned int u32;
typedef unsigned long long u64;

static u32 fb(float f) { union { float f; u32 u; } x; x.f = f; return x.u; }
static u64 db(double d) { union { double d; u64 u; } x; x.d = d; return x.u; }

static double ret_u(unsigned u) { return u; }
static float ret_ull(u64 v) { return v; }
static unsigned ret_du(double d) { return d; }
static u64 ret_ful(float f) { return f; }

static int inc(int x) { return x + 1; }
static int dbl(int x) { return x * 2; }
typedef struct { const char *name; const char *tag; int (*fn)(int); } def_t;
static const def_t defs[] = { { "a", "t", inc }, { "b", "t", dbl } };

static int explicit_casts(void) {
    volatile u64 one = 1, big = 0x8000000000000000ull, all = 0xffffffffffffffffull, p32 = 1ull << 32;
    volatile long long m1 = -1, nb = -5000000000ll;
    volatile unsigned u1 = 1, u31 = 0x80000000u, uall = 0xffffffffu;
    volatile float f35 = 3.5e9f, f15 = 1.5e19f, fn6 = -6e9f;
    volatile double d35 = 3.5e9, d15 = 1.5e19, d93 = 9.3e18;
    if (fb((float)(long long)one) != 0x3f800000u) return 1;
    if (fb((float)m1) != 0xbf800000u) return 2;
    if (fb((float)(long long)p32) != 0x4f800000u) return 3;
    if (fb((float)nb) != 0xcf9502f9u) return 4;
    if (fb((float)big) != 0x5f000000u) return 5;
    if (fb((float)all) != 0x5f800000u) return 6;
    if (fb((float)p32) != 0x4f800000u) return 7;
    if (db((double)big) != 0x43e0000000000000ull) return 8;
    if (db((double)all) != 0x43f0000000000000ull) return 9;
    if (db((double)nb) != 0xc1f2a05f20000000ull) return 10;
    if (db((double)u31) != 0x41e0000000000000ull) return 11;
    if (db((double)uall) != 0x41efffffffe00000ull) return 12;
    if (fb((float)u31) != 0x4f000000u) return 13;
    if (fb((float)uall) != 0x4f800000u) return 14;
    if (db((double)u1) != 0x3ff0000000000000ull) return 15;
    if (((unsigned)f35) != 0xd09dc300u) return 16;
    if (((unsigned)d35) != 0xd09dc300u) return 17;
    if (((u64)f15) != 0xd02ab50000000000ull) return 18;
    if (((u64)d15) != 0xd02ab486cedc0000ull) return 19;
    if (((u64)d93) != 0x81103cb9fb220000ull) return 20;
    if (((u64)(long long)fn6) != 0xfffffffe9a5f4400ull) return 21;
    return 0;
}

static int implicit_conversions(void) {
    volatile unsigned u = 3000000000u;
    volatile u64 ull = 0xfffffffffffff000ull;
    volatile long long ll = -5000000000ll;
    volatile int c = 1, i = 7;
    volatile double dd = 2.75;
    double d;
    float f;
    float ff;
    unsigned uu;
    u64 q;
    long long l;
    int k;
    d = u;             if (db(d) != 0x41e65a0bc0000000ull) return 31;
    f = u;             if (fb(f) != 0x4f32d05eu) return 32;
    d = ull;           if (db(d) != 0x43effffffffffffeull) return 33;
    f = ull;           if (fb(f) != 0x5f800000u) return 34;
    f = ll;            if (fb(f) != 0xcf9502f9u) return 35;
    d = u * 2.0;       if (db(d) != 0x41f65a0bc0000000ull) return 36;
    f = ll * 1.0f;     if (fb(f) != 0xcf9502f9u) return 37;
    d = 1.0; d += u;   if (db(d) != 0x41e65a0bc0200000ull) return 38;
    d = c ? ll : 0.5;  if (db(d) != 0xc1f2a05f20000000ull) return 39;
    d = c ? u : 0.5;   if (db(d) != 0x41e65a0bc0000000ull) return 40;
    if (db(ret_u(u)) != 0x41e65a0bc0000000ull) return 41;
    if (fb(ret_ull(ull)) != 0x5f800000u) return 42;
    if (ret_du(3.5e9) != 0xd09dc300u) return 43;
    if (ret_ful(1.5e19f) != 0xd02ab50000000000ull) return 44;
    uu = 3.5e9;        if (uu != 0xd09dc300u) return 45;
    d = 3.5e9; uu = d; if (uu != 0xd09dc300u) return 46;
    f = 3.5e9f; uu = f; if (uu != 0xd09dc300u) return 47;
    d = 1.5e19; q = d; if (q != 0xd02ab486cedc0000ull) return 48;
    f = 1.5e19f; q = f; if (q != 0xd02ab50000000000ull) return 49;
    f = -6e9f; l = f;  if ((u64)l != 0xfffffffe9a5f4400ull) return 50;
    ff = 1.5f;
    if (fb(ff * i) != 0x41280000u) return 51;
    if (fb(i * ff) != 0x41280000u) return 52;
    f = dd;            if (fb(f) != 0x40300000u) return 53;
    k = 10; k += 1.5;  if (k != 11) return 54;
    k = 10; k *= dd;   if (k != 27) return 55;
    uu = 10; uu += 3e9; if (uu != 0xb2d05e0au) return 56;
    return 0;
}

/* -3e9 is loop invariant: LICM hoists the constant into a callee-saved
 * pair, and fneg.d wrote the result back into that pair. */
static int invariant_neg(void) {
    double v[3];
    int i;
    int hits;
    v[0] = 1.5; v[1] = -2.5; v[2] = 1e10;
    hits = 0;
    for (i = 0; i < 3; i++) {
        double d;
        d = v[i];
        if (d > -3e9 && d < 2e9) hits = hits + 1;
    }
    if (hits != 2) return 61;
    return 0;
}

static int fnptr_table(void) {
    if (defs[0].fn != inc) return 71;
    if (defs[1].fn(5) != 10) return 72;
    return 0;
}

int main(void) {
    int r;
    r = explicit_casts();       if (r) return r;
    r = implicit_conversions(); if (r) return r;
    r = invariant_neg();        if (r) return r;
    r = fnptr_table();          if (r) return r;
    return 0;
}
