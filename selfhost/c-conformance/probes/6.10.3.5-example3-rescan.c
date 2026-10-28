// 6.10.3.4p2, 6.10.3.5  rescanning: redefinition, a name painted against re-expansion, a macro that opens a call, empty ## operands
#include <stdio.h>
static int k = 5;
static int tbl[2] = { 7, 2 };
static int sq(int v) { return v * v; }
static int id(int v) { return v; }
#define N 4
#define sq(e) sq(N + (e))        /* N is looked up at each use, not here */
#undef N
#define N 1
#define alias sq
#define tbl tbl[1]               /* object-like macro naming itself: not re-expanded */
#define open alias(2 *
#define id(e) e                  /* function-like macro naming a function */
#define cat(a, b) a ## b
#define TY() long
#define keep(v) v
int main(void) {
    int a = sq(k) + sq(sq(tbl));         /* sq(1+(5)) + sq(1+(sq(1+(tbl[1])))) */
    int b = alias(k - 1) + open 3);      /* sq(1+(k-1)) + sq(1+(2*3)) */
    int c = id(id)(9);                   /* the inner id is painted: the function is called */
    TY() arr[keep()] = { keep(8), cat(1,2), cat(3,), cat(,4) };
    printf("%d %d %d\n", a, b, c);
    printf("%d %ld %ld %ld %ld\n", (int)(sizeof arr / sizeof arr[0]), arr[0], arr[1], arr[2], arr[3]);
    return 0;
}
