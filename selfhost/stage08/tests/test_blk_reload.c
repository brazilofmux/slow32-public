/* Memory model step 5b.2: a spilled value read several times in one
 * block is reloaded once into a register for that block, not at each
 * read.  gather() keeps more words live across its loop than there are
 * registers, so some are spilled; in the loop body each spilled word is
 * read many times with no call between the reads (p->a .. p->h through
 * the pointer, and the words themselves).  run-tests.sh requires the
 * allocator to report a block reload (RELOAD line), and the results
 * must be right. */
struct S { int a, b, c, d, e, f, g, h; };

int gather(struct S *p, struct S *q, int n) {
    int s0 = p->a, s1 = p->b, s2 = p->c, s3 = p->d, s4 = p->e, s5 = p->f;
    int s6 = p->g, s7 = p->h, s8 = q->a, s9 = q->b, s10 = q->c, s11 = q->d;
    int s12 = q->e, s13 = q->f, s14 = q->g, s15 = q->h;
    int t0 = 1, t1 = 2, t2 = 3, t3 = 4, t4 = 5, t5 = 6, t6 = 7, t7 = 8;
    int i;
    for (i = 0; i < n; i++) {
        /* every spilled pointer and word read repeatedly in this block */
        s0 += p->a * q->h; s1 += p->b * q->g; s2 += p->c * q->f; s3 += p->d * q->e;
        s4 += p->e * q->d; s5 += p->f * q->c; s6 += p->g * q->b; s7 += p->h * q->a;
        s8 ^= s0 + s1; s9 ^= s2 + s3; s10 ^= s4 + s5; s11 ^= s6 + s7;
        s12 += s8 >> 3; s13 += s9 >> 3; s14 += s10 >> 3; s15 += s11 >> 3;
        t0 += s0 & 7; t1 += s1 & 7; t2 += s2 & 7; t3 += s3 & 7;
        t4 += s4 & 7; t5 += s5 & 7; t6 += s6 & 7; t7 += s7 & 7;
        p->a += t0; q->h -= t7;
    }
    return s0 ^ s1 ^ s2 ^ s3 ^ s4 ^ s5 ^ s6 ^ s7 ^ s8 ^ s9 ^ s10 ^ s11 ^
           s12 ^ s13 ^ s14 ^ s15 ^ t0 ^ t1 ^ t2 ^ t3 ^ t4 ^ t5 ^ t6 ^ t7;
}

int main(void) {
    struct S p, q;
    p.a = 3; p.b = 5; p.c = 7; p.d = 11; p.e = 13; p.f = 17; p.g = 19; p.h = 23;
    q.a = 29; q.b = 31; q.c = 37; q.d = 41; q.e = 43; q.f = 47; q.g = 53; q.h = 59;
    if (gather(&p, &q, 9) != 0x4c4) return 1;
    if (p.a != 190 || q.h != -151) return 2;
    return 0;
}
