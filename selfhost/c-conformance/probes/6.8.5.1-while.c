// 6.8.5.1p1  while tests before each iteration; controlling expr may be pointer or double
#include <stdio.h>
int main(void) {
    int n = 0, i = 10;
    while (i < 5) n += 1000;
    while (i) { n += i; i -= 3; if (i < 0) i = 0; }
    const char *s = "abcde", *p = s;
    while (*p) p++;
    double d = 3.0;
    int k = 0;
    while (d) { d -= 0.5; k++; }
    struct node { struct node *next; int v; } c = { 0, 3 }, b = { &c, 2 }, a = { &b, 1 };
    int sum = 0;
    for (struct node *q = &a; q; q = q->next) sum += q->v;
    struct node *q = &a;
    while (q) { sum += q->v * 10; q = q->next; }
    printf("%d %d %d %d\n", n, (int)(p - s), k, sum);
    return 0;
}
