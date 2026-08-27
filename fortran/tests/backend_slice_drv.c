/* C driver for the backend slice gate: calls the hand-built HIR
 * function and prints Sigma i^2, which the harness diffs against
 * the known-good expected output. */
#include <stdio.h>
int f77_slice(int n);
int main(void) {
    int i;
    for (i = 0; i <= 5; i++) printf("slice(%d) = %d\n", i, f77_slice(i));
    printf("slice(10) = %d\n", f77_slice(10));
    return 0;
}
