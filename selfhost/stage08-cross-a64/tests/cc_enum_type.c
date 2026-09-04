/* `enum` as a type specifier, in every context a declaration can appear.
 * Before the parser learned this, is_type() did not accept TK_ENUM, so
 * enum could only *define constants*: `enum tag x;` and
 * `enum { A, B } x = A;` both failed, and the bare form was handled by
 * two special cases that demanded a `;` right after the body. */

enum { ga_nothing, ga_loadlevel, ga_newgame } gameaction; /* doom's shape */
enum tag2 { C = 5, D };
enum { E1 = 7, E2 } gs = E2;                  /* bare def + initializer */

struct S { enum tag2 m; int n; };             /* as a struct member */

static int take(enum tag2 v) { return (int)v; }   /* as a parameter */

int main(void) {
    enum { X = 3, Y } s = Y;                  /* anonymous, local, init */
    enum tag2 t = D;                          /* tagged, local */
    struct S st;

    gameaction = ga_newgame;
    st.m = C;

    /* 4 + 6 + 5 + 4 + 8 + 2 + 7 = 36 */
    return s + take(t) + st.m + (int)sizeof(enum tag2) + gs + gameaction + E1;
}
