/* Struct-pointer-heavy code with adjacent field accesses.
 * Exercises SIB-fold patterns and load-after-store narrowing.
 *
 * Includes signed/unsigned narrow types to catch the narrow-load fold
 * bugs we fixed in commit f7f7725f / a327d0a6. */
struct narrow {
    unsigned char  b0;
    unsigned char  b1;
    unsigned char  b2;
    unsigned char  b3;
    unsigned short h0;
    unsigned short h1;
    signed char    sb;
    signed short   sh;
    int            w;
};

static int churn(struct narrow *p, int n) {
    int acc = 0;
    int i;
    for (i = 0; i < n; i++) {
        p->b0 = (p->b0 + 1) & 0xff;
        p->b1 = p->b0 ^ 0xa5;
        p->h0 = (p->h0 + 3) & 0xffff;
        p->h1 = p->h0 ^ 0xbeef;
        p->sb = (signed char)(p->sb - 1);
        p->sh = (signed short)(p->sh + 7);
        p->w  = p->w + (int)p->b0 + (int)p->h0 + (int)p->sb + (int)p->sh;
        acc = acc + p->w + (int)p->sb + (int)p->sh;
    }
    return acc;
}

int main(void) {
    struct narrow s = { 0xFE, 0, 0, 0, 0xFFF0, 0, -3, -7, 1000 };
    int r = churn(&s, 50);
    return r & 0xff;
}
