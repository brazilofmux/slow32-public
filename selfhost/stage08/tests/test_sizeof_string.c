/* selfhost ISSUES-84: sizeof of a string literal was 4, the size of the
 * pointer it decays to.  It is an array of char: the bytes and the NUL
 * (C90 6.1.4), in an expression, a constant expression, with and without
 * parentheses, after concatenation, with escapes. */
static char buf[sizeof("hello")];                 /* a constant context */
static const int k = sizeof "ab" + sizeof("");
static int pfx(const char *s) {
    const char *p = "--opt";
    int n = sizeof("--opt") - 1, i;
    for (i = 0; i < n; i++) if (s[i] != p[i]) return 0;
    return 1;
}
int main(void) {
    if (sizeof("A") != 2) return 1;
    if (sizeof("") != 1) return 2;
    if (sizeof "abcdefgh" != 9) return 3;
    if (sizeof("ab" "cd" "e") != 6) return 4;
    if (sizeof("\x41\n\0z") != 5) return 5;
    if (sizeof(buf) != 6) return 6;
    if (k != 4) return 7;
    if (!pfx("--opt=1") || pfx("-o")) return 8;
    if (sizeof("abc")[0] != 1) return 9;           /* an element is still a char */
    return 0;
}
