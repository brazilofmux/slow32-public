/* A value in two fp64 pairs -- wrong code.
 *
 * A double is two HIR values, and ra_build_pairs records each one's
 * partner: the CALL of an fp64 result with its CALLHI, and the lo and hi
 * arguments of each fp64 pseudo-call.  Clearing a double's sign through
 * a union keeps its lo word and makes a new hi, so the same lo value
 * arrives as the lo argument of the next subtraction with a different
 * partner.  The second pairing overwrote the first on one side only;
 * pair claiming and share-fate spilling then acted on the stale partner
 * and the subtraction read a hi word stored before it was computed.
 * fortran/'s f77 (same allocator) gave D2-ABS(D1) as D2; found by
 * regression/fuzz/ffuzz.py, seed 3.
 *
 * The values stay live across the printf calls, which is what puts the
 * pairs under pressure; a quieter version of this program passed with
 * the bug in place.  Returns 1 if the sum is wrong, 0 if it is right. */

#include <stdio.h>
union U { double d; unsigned w[2]; };
static void show(const char *s, double x) { printf("%s %.17g\n", s, x); }
int main(void) {
    volatile double v1 = 487.0, v2 = 150.53, v3 = 187.06;
    double d1 = -v1, d2 = -v2, d3 = v3, td;
    union U u;
    show("a", d1); show("b", d2); show("c", d3);
    u.d = d1;
    u.w[1] &= 0x7fffffffu;            /* |d1|: d1's lo, a new hi */
    td = d2 - u.d;
    u.d = d2;
    u.w[1] &= 0x7fffffffu;
    td = td + (d2 < u.d ? d2 : u.d);  /* min(d2, |d2|) */
    show("a", d1); show("b", d2); show("c", d3);
    if (td != (-v2 - v1) - v2) return 1;
    return 0;
}
