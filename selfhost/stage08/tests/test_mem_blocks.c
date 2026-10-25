/* memcpy and memset (hand-written in mmio_no_start.s) for every length
 * 0..70 at four source and four destination misalignments, against a
 * byte loop; guard bytes on both sides of the destination must survive.
 * The 16-byte, word and byte paths all meet a boundary here.
 * slow32-dbt recognizes both and runs them natively unless given -I,
 * so the guest code is exercised by the interpreter and qemu legs of
 * regression/run-kit-differential.sh, not by run-tests.sh's engine. */
void *memcpy(void *d, const void *s, unsigned int n);
void *memset(void *d, int c, unsigned int n);

static unsigned char src[96];
static unsigned char dst[112];
static unsigned char want[112];

int main(void) {
    int n;
    int so;
    int dof;
    int i;
    unsigned char *r;
    for (i = 0; i < 96; i++) src[i] = (unsigned char)(i * 37 + 11);
    for (n = 0; n <= 70; n++) {
        for (so = 0; so < 4; so++) {
            for (dof = 0; dof < 4; dof++) {
                for (i = 0; i < 112; i++) { dst[i] = 0xa5; want[i] = 0xa5; }
                for (i = 0; i < n; i++) want[8 + dof + i] = src[so + i];
                r = memcpy(dst + 8 + dof, src + so, (unsigned)n);
                if (r != dst + 8 + dof) return 1;
                for (i = 0; i < 112; i++) if (dst[i] != want[i]) return 2;

                for (i = 0; i < 112; i++) { dst[i] = 0x3c; want[i] = 0x3c; }
                for (i = 0; i < n; i++) want[8 + dof + i] = 0xd2;
                r = memset(dst + 8 + dof, 0x7d2, (unsigned)n);   /* only the low byte counts */
                if (r != dst + 8 + dof) return 3;
                for (i = 0; i < 112; i++) if (dst[i] != want[i]) return 4;
            }
        }
    }
    return 0;
}
