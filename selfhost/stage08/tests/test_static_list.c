/* Block-scope static declarator lists (regal's test_keyword.c:
 * `static char a[4096], b[4096];`).  Scalars already looped; arrays did
 * not.  Both arrays must be distinct storage that persists. */
#include <string.h>
static int fill(int k) {
    static char a[64], b[64];
    static int n, m;
    if (k == 0) { strcpy(a, "first"); strcpy(b, "second"); n = 1; m = 2; return 0; }
    if (strcmp(a, "first") != 0) return 1;
    if (strcmp(b, "second") != 0) return 2;
    if (a == b) return 3;
    return n + m;   /* 3 */
}
int main(void) {
    if (fill(0) != 0) return 10;
    if (fill(1) != 3) return 20;
    return 0;
}
