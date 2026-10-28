// 6.10.3.4p2  m(m): the inner m, met during m's own rescan, is not replaced again
#include <stdio.h>
static int m(int a, int b) { return a * 10 + b; }
static int one(int a) { return a + 500; }
#define w 0,1
#define m(a) a(w)
#define n(a) a(2)
int main(void) {
    printf("%d %d\n", m(m), n(one));
    return 0;
}
