/* A ?: of two structures is that structure (6.5.15p5).  sema typed it as
 * an int: a function returning `k ? a : b` gave back the chosen arm's
 * address and copied nothing into the caller's buffer -- the arm was a
 * dead local, or the caller's buffer stayed empty -- and passing such a
 * ?: by value sent one word.  SLOW-32 xBase's runtime, built by stage08,
 * printed "Error ???/???" for every error because of it
 * (selfhost ISSUES-104). */
typedef struct { int type; int pad; char str[256]; } V;
static V tab[8];

static V get(int k) {
    V *p = (k >= 0 && k < 8) ? &tab[k] : 0;
    V nil;
    nil.type = 0;
    return p ? *p : nil;
}

static V pick(int k) { V nil; nil.type = 77; return k < 8 ? tab[k] : nil; }
static int show(V v) { return v.type * 10; }

int main(void) {
    V a, b, c, x;
    int k;
    for (k = 0; k < 8; k++) { tab[k].type = k + 1; tab[k].str[0] = (char)('a' + k); tab[k].str[1] = 0; }
    a = get(0); b = get(7); c = get(9);
    if (a.type != 1 || b.type != 8 || c.type != 0 || b.str[0] != 'h') return 1;
    if (pick(4).type != 5 || pick(9).type != 77) return 2;
    k = 1;
    x = k ? tab[1] : tab[2];
    if (x.type != 2) return 3;
    if (show(k ? tab[2] : tab[1]) != 30 || show((k - 1) ? tab[2] : tab[1]) != 20) return 4;
    if ((k ? tab[3] : tab[4]).type != 4) return 5;
    return 0;
}
