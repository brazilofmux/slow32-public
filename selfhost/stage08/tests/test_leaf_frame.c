/* GitHub issue 73: leaf/FP elision.  GitHub issue 72: last RET falls
 * into the epilogue (withcall must not jal r0 to the next label).
 * add1 is a pure register leaf — no frame.
 * sumarr has a local array: frame but no r30 (SP-relative).
 * ident is a called leaf; withcall has a local and a call — saves lr,
 * not r30. */
int add1(int x) { return x + 1; }
int sumarr(void) { int a[4]; a[0] = 1; a[1] = 2; return a[0] + a[1]; }
int ident(int x) { return x; }
int withcall(int x) {
    int a[2];
    a[0] = x;
    return ident(a[0]) + 1;
}

int main(void) {
    if (add1(41) != 42) return 1;
    if (sumarr() != 3) return 2;
    if (withcall(7) != 8) return 3;
    return 0;
}
