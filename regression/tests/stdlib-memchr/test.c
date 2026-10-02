/* memchr, every way it can answer.
 *
 * slow32-dbt runs memchr as a native routine (tools/dbt: the stub
 * emit_native_memchr_stub and its arm64 twin), as it does memcpy and
 * memcmp; the reference interpreter runs runtime/string.c.  The two must
 * agree on what is returned for every call a program can make, so this
 * prints the OFFSET each call found (or "none"), not a pass or a fail:
 * under the differential harness every engine's stub is compared with
 * the reference's loop.
 *
 *   - the byte first, last, in the middle, absent; twice (the first wins);
 *   - a count of zero, with a pointer that is not even memory;
 *   - a count that stops one short of the byte, and exactly at it;
 *   - the byte given as an int with more than eight bits (it is converted
 *     to unsigned char), and bytes of 128 and above;
 *   - every alignment of the start, and counts across word boundaries;
 *   - a count far beyond the buffer -- past the end of memory itself --
 *     with the byte present: found, and no fault, because memchr reads no
 *     further than the match (memchr(p, 0, SIZE_MAX) is rawmemchr).
 *
 * The buffer is filled at run time and reached through a volatile zero,
 * so no call can be folded away.
 */
#include <stdio.h>
#include <string.h>
#include <stdint.h>

static volatile int z = 0;
static unsigned char buf[300];

static void show(const char *what, const void *r, const unsigned char *base)
{
    if (r) printf("%-28s %d\n", what, (int)((const unsigned char *)r - base));
    else printf("%-28s none\n", what);
}

int main(void)
{
    unsigned char *b = buf + z;
    for (int i = 0; i < 300; i++) b[i] = (unsigned char)('a' + i % 23);     /* 'a'..'w', no 'x', 'y', 'z' */

    show("first byte", memchr(b, 'a', 300), b);
    show("last byte", memchr(b + 290, b[299], 10 + (size_t)z), b);
    b[150] = 'x';
    show("in the middle", memchr(b, 'x', 300), b);
    show("absent", memchr(b, 'z', 300), b);
    b[200] = 'x';
    show("twice: the first", memchr(b, 'x', 300), b);
    show("from past the first", memchr(b + 151, 'x', 149), b);
    show("count zero", memchr(b, 'a', (size_t)z), b);
    show("count zero, no memory", memchr((void *)(uintptr_t)(0xF0000000u + (unsigned)z), 'a', (size_t)z), b);
    show("one short of it", memchr(b, 'x', 150), b);
    show("exactly at it", memchr(b, 'x', 151), b);
    show("an int of more bits", memchr(b, 0x100 + 'x' + z, 300), b);
    show("a negative int", memchr(b, -256 + 'x' + z, 300), b);
    b[77] = 0x80; b[78] = 0xFF; b[79] = 0;
    show("0x80", memchr(b, 0x80, 300), b);
    show("0xFF", memchr(b, 0xFF, 300), b);
    show("0xFF as -1", memchr(b, -1 + z, 300), b);
    show("zero", memchr(b, 0, 300), b);

    /* every alignment of the start, short counts across word boundaries */
    unsigned sum = 0;
    for (int off = 0; off < 9; off++)
        for (int n = 0; n < 20; n++) {
            const unsigned char *r = memchr(b + 100 + off, 'h' + z, (size_t)n);
            sum = sum * 31u + (r ? (unsigned)(r - b) : 999u);
        }
    printf("%-28s %u\n", "alignments and counts", sum);

    /* a count past the end of memory, the byte present: no fault */
    show("count past memory", memchr(b, 'x', (size_t)0xFFFFFFFFu - (size_t)z), b);
    show("... of zero bytes", memchr(b, 0, (size_t)0x7FFFFFFF + (size_t)z), b);
    return 0;
}
