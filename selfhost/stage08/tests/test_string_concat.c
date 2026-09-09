/* GitHub issue 54: adjacent string literals concatenate.
 * GitHub issue 42 / selfhost ISSUES-67: the concat index was a
 * 64-entry local; SQLite's shell builds one statement from hundreds
 * of adjacent literals.  80 > 64.  1025 is still a hard error; the
 * array is static because 4KB per frame of the recursive expression
 * parser overran the compiler's stack. */
static char *g = "a" "b" "c";
static char lots[] =
    "x" "x" "x" "x" "x" "x" "x" "x" "x" "x"
    "x" "x" "x" "x" "x" "x" "x" "x" "x" "x"
    "x" "x" "x" "x" "x" "x" "x" "x" "x" "x"
    "x" "x" "x" "x" "x" "x" "x" "x" "x" "x"
    "x" "x" "x" "x" "x" "x" "x" "x" "x" "x"
    "x" "x" "x" "x" "x" "x" "x" "x" "x" "x"
    "x" "x" "x" "x" "x" "x" "x" "x" "x" "x"
    "x" "x" "x" "x" "x" "x" "x" "x" "x" "x";

int main(void) {
    char *p;
    p = "foo" "bar" "baz";
    if (g[0] != 'a' || g[1] != 'b' || g[2] != 'c' || g[3] != 0) return 1;
    if (p[0] != 'f' || p[3] != 'b' || p[6] != 'b' || p[9] != 0) return 2;
    if (sizeof("x" "y" "z") != 4) return 3;
    if (sizeof(lots) != 81) return 4;
    if (lots[0] != 'x' || lots[79] != 'x' || lots[80] != 0) return 5;
    return 0;
}
