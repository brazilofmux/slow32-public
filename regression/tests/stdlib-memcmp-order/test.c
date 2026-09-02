/* memcmp's ORDERING, not just its equality.
 *
 * Every other memcmp in this suite is written `memcmp(...) == 0`, which
 * cannot see the sign or the magnitude of the result.  That blind spot is
 * real: mutating the DBT's native memcmp stub to invert its sign, or to
 * skip byte 0, left run-kit-tools-differential.sh reporting 21/21 AGREE
 * (measured 2026-09-02).  Only a stub returning 0 unconditionally was
 * caught, and then only by a COBOL program's output.
 *
 * So this prints the RETURNED VALUE of each comparison rather than a
 * PASS/FAIL.  Under the differential harness that makes every engine
 * byte-compare its arithmetic against the reference interpreter, so a
 * stub that gets the sign right and the magnitude wrong still diverges.
 * The contract being pinned is runtime/string.c: the difference of the
 * first unequal pair, read as unsigned char, or 0.
 *
 * Buffers are filled at run time and indexed through a volatile zero so
 * that no constant-folding of memcmp can quietly replace the call these
 * cases exist to make.
 */
#include <stdio.h>
#include <string.h>

static volatile int z = 0;          /* opaque 0: defeats constant folding */
static unsigned char a[80], b[80];

static void fill(const char *pa, const char *pb, int n)
{
    for (int i = 0; i < n; i++) { a[i] = (unsigned char)pa[i]; b[i] = (unsigned char)pb[i]; }
}

static void show(const char *label, int n)
{
    printf("%-28s %d\n", label, memcmp(a + z, b + z, (size_t)n));
}

int main(void)
{
    /* equality, and the degenerate length */
    fill("abc", "abc", 3);            show("equal", 3);
    fill("abc", "xyz", 3);            show("length 0", 0);

    /* sign, both directions -- an inverted stub flips exactly these */
    fill("a", "b", 1);                show("a<b", 1);
    fill("b", "a", 1);                show("b>a", 1);

    /* the difference is ONLY in byte 0 -- a stub starting at byte 1 sees 0 */
    fill("Xbcdefgh", "Ybcdefgh", 8);  show("differs at 0 only", 8);

    /* FIRST difference wins, and the two differences have opposite signs:
     * a stub reporting the last one returns +25 instead of -1 */
    fill("azz", "baa", 3);            show("first diff not last", 3);

    /* bytes are UNSIGNED: 0x80 vs 0x01 is positive, not negative */
    fill("\x80", "\x01", 1);          show("0x80 vs 0x01", 1);
    fill("\x01", "\x80", 1);          show("0x01 vs 0x80", 1);
    fill("\xff", "\x00", 1);          show("0xff vs 0x00", 1);

    /* exact magnitude, not merely the sign */
    fill("a", "A", 1);                show("magnitude 32", 1);
    fill("\xfe", "\x01", 1);          show("magnitude 253", 1);

    /* the difference walks across word and vector boundaries */
    fill("0123456789abcdef", "9123456789abcdef", 16); show("diff at index 0", 16);
    fill("0123456789abcdef", "0923456789abcdef", 16); show("diff at index 1", 16);
    fill("0123456789abcdef", "0129456789abcdef", 16); show("diff at index 3", 16);
    fill("0123456789abcdef", "0123956789abcdef", 16); show("diff at index 4", 16);
    fill("0123456789abcdef", "0123456989abcdef", 16); show("diff at index 7", 16);
    fill("0123456789abcdef", "0123456799abcdef", 16); show("diff at index 8", 16);
    fill("0123456789abcdef", "0123456780abcdef", 16); show("diff at index 9", 16);
    fill("0123456789abcdef", "0123456789abcdeg", 16); show("diff at last byte", 16);

    /* a long run of equal bytes before the difference */
    {
        int i;
        for (i = 0; i < 64; i++) { a[i] = 'q'; b[i] = 'q'; }
        b[63] = 'r';
        printf("%-28s %d\n", "diff at index 63 of 64", memcmp(a + z, b + z, 64));
        b[63] = 'q';
        printf("%-28s %d\n", "64 equal", memcmp(a + z, b + z, 64));
    }

    /* the compared region stops at n: a difference past it is invisible */
    fill("abcZ", "abcY", 4);          show("difference past n", 3);

    return 0;
}
