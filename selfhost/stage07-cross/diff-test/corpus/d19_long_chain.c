/* Long sequential dependency chain in a single basic block.
 * Each step depends on the previous, so the regalloc must shepherd
 * one value through ~30 steps without losing it.  Mixed with
 * occasional uses of older values to keep them live and force
 * register pressure decisions. */
static unsigned int chain(unsigned int x) {
    unsigned int a = x + 1;
    unsigned int b = a ^ 0x55;
    unsigned int c = b - 7;
    unsigned int d = c * 3;
    unsigned int e = d + a;          /* re-use a */
    unsigned int f = e ^ b;          /* re-use b */
    unsigned int g = f << 1;
    unsigned int h = g + c;          /* re-use c */
    unsigned int i = h - d;          /* re-use d */
    unsigned int j = i * 5;
    unsigned int k = j ^ e;          /* re-use e */
    unsigned int l = k + f;          /* re-use f */
    unsigned int m = l - g;          /* re-use g */
    unsigned int n = m * 7;
    unsigned int o = n ^ h;          /* re-use h */
    unsigned int p = o + i;          /* re-use i */
    unsigned int q = p - j;          /* re-use j */
    unsigned int r = q * 11;
    unsigned int s = r ^ k;          /* re-use k */
    unsigned int t = s + l;          /* re-use l */
    unsigned int u = t - m;          /* re-use m */
    unsigned int v = u * 13;
    unsigned int w = v ^ n;          /* re-use n */
    unsigned int xx = w + o;         /* re-use o */
    return a ^ b ^ c ^ d ^ e ^ f ^ g ^ h ^ i ^ j ^
           k ^ l ^ m ^ n ^ o ^ p ^ q ^ r ^ s ^ t ^
           u ^ v ^ w ^ xx;
}

int main(void) {
    unsigned int acc = 0;
    int i;
    for (i = 0; i < 64; i++) {
        acc = acc * 17u + chain((unsigned int)i * 0xDEADBEEFu);
    }
    return acc & 0xff;
}
