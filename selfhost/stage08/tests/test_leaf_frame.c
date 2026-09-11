/* GitHub issue 73: a leaf with no stack data omits the r31/r30 frame.
 * add1 is a pure register leaf. sumarr has a local array and must keep
 * a frame (but still not a call, so it may skip the lr save). */
int add1(int x) { return x + 1; }
int sumarr(void) { int a[4]; a[0] = 1; a[1] = 2; return a[0] + a[1]; }

int main(void) {
    if (add1(41) != 42) return 1;
    if (sumarr() != 3) return 2;
    return 0;
}
