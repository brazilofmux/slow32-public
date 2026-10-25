/* A declarator after a comma brings its own stars and qualifiers and
 * starts from the declaration's type before the first one's stars:
 * with a typedef'd pointer `name a, b;` b is a pointer too (it was
 * taken for the char it points at), and block-scope statics accept
 * `*const b` (they refused any star).  An address constant naming a
 * static local uses that local's own symbol (the link failed on a bare
 * `x`).  The target is predefined as clang spells it. */
typedef char *name;
typedef const int *cip;

static int sizes(void) {
    name a = "red", b = "teal";
    static name c = "plum", d = "sage";
    return (int)sizeof(a) + (int)sizeof(b) * 10 + (int)sizeof(c) * 100 +
           (int)sizeof(d) * 1000 + (b[1] == 'e') + (d[3] == 'e') * 2;
}

static int stars(void) {
    static const char *const one = "lark", *const two = "wren";
    static const char *three = "kite", **four = &three;
    static int base = 40, *pb = &base, **ppb = &pb;
    static char cells[3] = { 'o', 'w', 'l' }, *pc = cells;
    static cip q = &base, r = &base;
    return one[0] + two[1] + (*four)[2] + **ppb + pc[2] + *q + *r;
}

#if defined(__SLOW32__) && defined(__slow32__) && __SLOW32__ == 1
#define TARGET_OK 1
#else
#define TARGET_OK 0
#endif

int main(void) {
    if (sizes() != (int)sizeof(char *) * 1111 + 3) return 1;
    if (stars() != 'l' + 'r' + 't' + 40 + 'l' + 40 + 40) return 2;
    if (!TARGET_OK) return 3;
    return 0;
}
