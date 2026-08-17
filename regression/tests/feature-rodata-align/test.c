/* Guards the .p2align fix: the assembler must honor the alignment
 * directives LLVM emits inside merged .rodata. When .p2align was
 * silently ignored, a const word array following odd-length char
 * arrays landed on an odd address (unaligned loads happen to work on
 * the C emulators, so only address checks catch it).
 *
 * The addresses are laundered through volatile pointers: at -O2 LLVM
 * folds `(uintptr_t)words & 3` to 0 — it assumes the alignment it
 * asked the assembler for, which is exactly the assumption the bug
 * violated. */
#include <stdio.h>
#include <stdint.h>

/* Odd-length named char arrays sit in .rodata itself, in definition
 * order, so the word arrays after them depend on .p2align being
 * honored — no reliance on section emission order. */
static const char pad1[5] = "odd.";
static const uint32_t words[4] = { 0x11111111, 0x22222222, 0x33333333, 0x44444444 };
static const char pad2[7] = "odder.";
static const uint16_t halves[3] = { 0x1111, 0x2222, 0x3333 };
static const char pad3[3] = "od";
static const uint64_t dwords[2] = { 0x1111111122222222ull, 0x3333333344444444ull };

static const uint32_t * volatile vwords = words;
static const uint16_t * volatile vhalves = halves;
static const uint64_t * volatile vdwords = dwords;

int main(void) {
    const uint32_t *w = vwords;
    const uint16_t *h = vhalves;
    const uint64_t *d = vdwords;

    printf("%s\n", pad1);
    printf("%s\n", pad2);
    printf("%s\n", pad3);
    if (((uintptr_t)w & 3u) != 0) {
        printf("FAIL words %p\n", (const void *)w);
        return 1;
    }
    if (((uintptr_t)h & 1u) != 0) {
        printf("FAIL halves %p\n", (const void *)h);
        return 1;
    }
    if (((uintptr_t)d & 3u) != 0) {
        printf("FAIL dwords %p\n", (const void *)d);
        return 1;
    }
    printf("aligned %x %x %x\n", w[0], h[1], (unsigned)(d[1] >> 32));
    return 0;
}
