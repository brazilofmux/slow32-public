/* GitHub issue 54: adjacent string literals concatenate.  The index
 * array is 1024 and static (selfhost ISSUES-67); 1025 is a hard error.
 * Three literals keep the path alive without filling the array. */
static char *g = "a" "b" "c";

int main(void) {
    char *p;
    p = "foo" "bar" "baz";
    if (g[0] != 'a' || g[1] != 'b' || g[2] != 'c' || g[3] != 0) return 1;
    if (p[0] != 'f' || p[3] != 'b' || p[6] != 'b' || p[9] != 0) return 2;
    if (sizeof("x" "y" "z") != 4) return 3;
    return 0;
}
