/* strncpy must write exactly n bytes. The old implementation wrote
 * n+1 when the source was shorter (the NUL-copy iteration skipped its
 * n--), zeroing the low byte of whatever lived after the field. A
 * WAD directory's name[8] followed by a pointer found it. */
#include <stdio.h>
#include <string.h>

static struct {
    char name[8];
    unsigned sentinel;
} rec;

int main(void) {
    int i;
    rec.sentinel = 0xA5A5A5A5u;
    strncpy(rec.name, "PLAYPAL", 8);          /* 7 chars: pads 1 NUL */
    if (rec.sentinel != 0xA5A5A5A5u) {
        printf("FAIL sentinel %08x\n", rec.sentinel);
        return 1;
    }
    strncpy(rec.name, "AB", 8);               /* short: pads 6 NULs */
    if (rec.sentinel != 0xA5A5A5A5u) {
        printf("FAIL sentinel2 %08x\n", rec.sentinel);
        return 1;
    }
    for (i = 2; i < 8; i++) {
        if (rec.name[i] != 0) {
            printf("FAIL pad %d\n", i);
            return 1;
        }
    }
    strncpy(rec.name, "EXACTLY8", 8);         /* len==n: no NUL, no pad */
    if (rec.sentinel != 0xA5A5A5A5u || rec.name[7] != '8') {
        printf("FAIL exact\n");
        return 1;
    }
    printf("strncpy-ok\n");
    return 0;
}
