/* Global aggregate initializers, including as the FIRST declarator.
 *
 * Regression for the PARSE-TOP-LEVEL hang. PARSE-TOP-LEVEL parses the first
 * declarator itself (it has to, in order to tell a function from a variable),
 * so it carried an inlined copy of the initializer emitter and delegated only
 * the post-comma declarators to PARSE-GLOBAL-VAR. The inlined copy handled
 * TK-NUM and TK-STR but had no P-LBRACE case, so a leading
 *
 *     int a[3] = {1, 2, 3};
 *
 * fell through to PARSE-ASSIGN-EXPR, which does not consume '{', does not
 * advance the token, and therefore looped forever. Writing a dummy variable
 * first was a working "fix" — it pushed the array past the comma and into
 * PARSE-GLOBAL-VAR, which had the brace handling all along.
 *
 * Both sites now call PARSE-GLOBAL-INIT.
 *
 * Deliberately NOT covered here: negative literals in an aggregate
 * (`{-7, 3}`). They compile but emit the wrong bytes — `-` is TK-PUNCT, so it
 * is caught by the outer TK-PUNCT branch and emitted as "unknown punctuation"
 * (.word 0), leaving the P-MINUS arm downstream unreachable. Pre-existing,
 * present on both paths, tracked separately.
 */

int    a[3] = {1, 2, 3};          /* first declarator — the hang */
static int b[2] = {9, 8};         /* static, first declarator */
int    d, e[2] = {5, 6};          /* after a comma (always worked) */
char  *s[2] = {"x", "y"};         /* string literals */

int main(void) {
    if (a[0] != 1) return 1;
    if (a[1] != 2) return 2;
    if (a[2] != 3) return 3;

    if (b[0] != 9) return 4;
    if (b[1] != 8) return 5;

    if (e[0] != 5) return 6;
    if (e[1] != 6) return 7;

    if (s[0][0] != 'x') return 8;
    if (s[1][0] != 'y') return 9;

    return 0;
}
