/* memchr from an address that is not memory: a fault, at that address.
 * (bug-dbt-intrinsic-bounds-memchr has the search that runs off the end;
 * this is the one that never begins.  slow32-dbt runs memchr natively and
 * must not hand the host's memchr a pointer outside the guest.) */
#include <stdio.h>
#include <string.h>
#include <stdint.h>

static volatile int z = 0;

int main(void)
{
    printf("before\n");
    printf("%s\n", memchr((void *)(uintptr_t)(0x20000000u + (unsigned)z), 'a', 4 + (size_t)z) ? "found" : "none");
    return 0;
}
