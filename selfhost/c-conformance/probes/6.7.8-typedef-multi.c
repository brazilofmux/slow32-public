// 6.7.8p3/6.7p1  one typedef declaring several names (T, *TP)
#include <stdio.h>
typedef int A, *AP;
typedef struct { int x, y; } Pt, *PtP;
int main(void) { A a = 3; AP p = &a; Pt q = { 4, 5 }; PtP pq = &q; printf("%d %d\n", *p, pq->y); return 0; }
