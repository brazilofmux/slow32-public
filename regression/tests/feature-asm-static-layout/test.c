/* Pins the .comm/.bss cursor bug: a file mixing zero-initialized
 * statics (emitted as .bss section data) with uninitialized statics
 * and tentative globals (emitted as .local/.comm) used to get
 * overlapping BSS addresses — Doom's hu_stuff had plr sharing a cell
 * with chat_on, and the crash took an afternoon to find. Every
 * variable here must have distinct storage. */
#include <stdio.h>

static int zeroed_a = 0;         /* .bss-section style */
static void *uninit_ptr;         /* .local + .comm style */
static int zeroed_b = 0;
static char small;               /* 1-byte comm: alignment matters   */
int tentative_global;            /* plain .comm                      */
static long long wide;           /* 8-byte alignment                 */
int other_global;

int main(void) {
    void *addrs[7] = { &zeroed_a, &uninit_ptr, &zeroed_b, &small,
                       &tentative_global, &wide, &other_global };
    int i, j;

    zeroed_a = 0x11111111;
    uninit_ptr = (void *)0x22222222;
    zeroed_b = 0x33333333;
    small = 0x44;
    tentative_global = 0x55555555;
    wide = 0x6666666677777777ll;
    other_global = 0x88888888u;

    for (i = 0; i < 7; i++) {
        for (j = i + 1; j < 7; j++) {
            if (addrs[i] == addrs[j]) {
                printf("FAIL alias %d %d\n", i, j);
                return 1;
            }
        }
    }
    if (zeroed_a != 0x11111111 || uninit_ptr != (void *)0x22222222 ||
        zeroed_b != 0x33333333 || small != 0x44 ||
        tentative_global != 0x55555555 ||
        wide != 0x6666666677777777ll ||
        (unsigned)other_global != 0x88888888u) {
        printf("FAIL stomp\n");
        return 1;
    }
    /* SLOW-32 ABI aligns i64 to 4 (i386-style). */
    if (((unsigned long)(void *)&wide & 3u) != 0) {
        printf("FAIL align\n");
        return 1;
    }
    printf("layout-ok\n");
    return 0;
}
