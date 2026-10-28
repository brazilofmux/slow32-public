// 6.5.2p1  postfix operators chain left to right: call()[i].m->n++, (*fp)(x)(y)
#include <stdio.h>
struct N { int v; struct N *next; };
static struct N n2 = {20, 0}, n1 = {10, &n2};
static struct N tbl[2];
static struct N *get(void) { tbl[1] = n1; return tbl; }
static int add1(int x) { return x + 1; }
typedef int (*fn)(int);
static fn pick(int i) { (void)i; return add1; }
int main(void) {
    get()[1].next->v++;
    printf("%d %d %d\n", n2.v, pick(0)(5), (*pick(1))(6));
    return 0;
}
