/* memchr where memory ends.
 *
 * memchr reads as far as the first match and no further.  A search of
 * the last bytes of memory for a byte they do not hold, with a count
 * that stops where memory does, finds nothing; with a count of one more
 * it reads the first address past memory, and that is a fault -- at that
 * address.  slow32-dbt runs memchr natively (tools/dbt,
 * emit_native_memchr_stub and its arm64 twin) and must come to both
 * answers: it searches what memory there is, and when the byte is not
 * there and the count wanted more, takes the fault the guest's own loop
 * would have taken.
 *
 * The byte looked for is chosen by reading those last 64 bytes -- they
 * are the top of the stack, and what they hold is the program's business
 * -- and is not printed.
 */
#include <stdio.h>
#include <string.h>
#include <stdint.h>

static volatile int z = 0;

int main(void)
{
    unsigned char *p = (unsigned char *)(uintptr_t)(0x0FFFFFC0u + (unsigned)z);
    int c = -1;
    for (int v = 1; v < 256 && c < 0; v++) {
        int seen = 0;
        for (int i = 0; i < 64; i++) if (p[i] == v) seen = 1;
        if (!seen) c = v;
    }
    printf("to the end of memory: %s\n", memchr(p, c, 64 + (size_t)z) ? "found" : "none");
    printf("one byte past it: %s\n", memchr(p, c, 65 + (size_t)z) ? "found" : "none");
    return 0;
}
