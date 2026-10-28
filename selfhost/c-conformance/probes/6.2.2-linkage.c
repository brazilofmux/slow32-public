// 6.2.2p4  extern after static keeps internal linkage; block-scope extern refers to the file object
#include <stdio.h>
static int s = 5;
extern int s;
static int f(void);
int f(void) { return 11; }       /* no storage class: takes the prior internal linkage */
int g;                           /* tentative */
int g = 4;
int main(void) {
    extern int s;
    int k;
    { extern int g; k = g; }
    printf("%d %d %d\n", s, f(), k);
    return 0;
}
