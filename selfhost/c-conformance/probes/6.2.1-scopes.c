// 6.2.1p4  block scope hides outer; scope starts after the declarator; for-scope
#include <stdio.h>
int x = 1;
int main(void) {
    int r = 0;
    int x = 2;
    r += x;                       /* 2 */
    {
        int x = sizeof x + 6;     /* x in scope from its own declarator: sizeof the new int */
        r += x;                   /* 10 */
    }
    for (int x = 100; x < 101; x++) r += x;  /* 100 */
    r += x;                       /* 2 */
    enum { A = sizeof r, B = A + 1 };
    printf("%d %d\n", r, B);
    return 0;
}
