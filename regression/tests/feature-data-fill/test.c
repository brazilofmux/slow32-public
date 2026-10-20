/* Initialized data with runs of 0xFF bytes: LLVM writes them as
 * `.zero N,255`, and the assembler ignored the fill, so each such run
 * came out 0.  A table of {-1, -1} pairs then read {0, 0} (TinyMUX's
 * colour k-d tree walked a cycle until the stack ran out).  Arrays,
 * structs and a long run, read back here; the expected values are the
 * initializers. */
#include <stdio.h>
#include <string.h>

struct node { int key; int child[2]; short tag; };

static struct node tree[] = {
    { 10, { 1, 2 }, 7 },
    { 20, { -1, -1 }, -1 },
    { 30, { -1, -1 }, 3 },
};
static int runs[] = { 5, -1, -1, -1, -1, -1, -1, -1, -1, 6 };
static unsigned char bytes[] = { 1, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 2 };
static long long wide[] = { -1, -1, 9 };
static const signed char ro[] = { -1, -1, -1, -1, 4 };

int main(void) {
    int sum = 0;
    for (int i = 0; i < 3; i++)
        printf("node %d: key %d child %d %d tag %d\n", i, tree[i].key, tree[i].child[0], tree[i].child[1], tree[i].tag);
    for (int i = 0; i < 10; i++) sum += runs[i];
    printf("runs sum %d (5 + 8*-1 + 6 = 3)\n", sum);
    unsigned acc = 0;
    for (unsigned i = 0; i < sizeof bytes; i++) acc = acc * 31 + bytes[i];
    printf("bytes %u, ff count %d\n", acc, (int)(memchr(bytes, 2, sizeof bytes) ? 7 : 0));
    printf("wide %lld %lld %lld\n", wide[0], wide[1], wide[2]);
    printf("ro %d %d %d %d %d\n", ro[0], ro[1], ro[2], ro[3], ro[4]);
    return 0;
}
