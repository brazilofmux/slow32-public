/* A post-increment through a loop-carried pointer, read after the
 * increment: `ch = *p++` lowers to  old = phi; new = old + 1; LOAD old,
 * so the phi result and its back-edge arg overlap inside the body.
 * gc_drop_phi_edges relaxed exactly that interference and the allocator
 * coalesced them -- `add x4,x4,#1` ran before `ldrb [x4]`, and the loop
 * read its input shifted by one byte (GitHub issue 80: libutf's NFC
 * quick-check said U+0958 was already NFC).  Straight-line `*p++` was
 * never affected; only the loop shape, where p is a phi.  Registered on
 * the --hir rule: the plain rule is the --tree reference, whose own *p++
 * bug (GitHub issue 81) segfaults here for an unrelated reason. */

static int walk(const unsigned char *p, const unsigned char *e) {
    int acc = 0;
    while (p < e) { unsigned char ch = *p++; acc = acc * 4 + ch; }
    return acc;
}

/* Same, with a second loop-carried value in the condition -- the
 * RunIntegerDFA_u16 shape. */
static int walk2(const unsigned char *p, const unsigned char *e, int st, int lim) {
    int acc = 0;
    while (p < e && st < lim) { unsigned char ch = *p++; acc = acc * 4 + ch; st = st + 1; }
    return acc;
}

int main(void) {
    unsigned char s[3];
    s[0] = 1; s[1] = 2; s[2] = 3;
    /* each is 1*16 + 2*4 + 3 = 27; the bug gave 44 (bytes 2,3,<past end>) */
    return walk(s, s + 3) + walk2(s, s + 3, 0, 100);   /* 54 */
}
