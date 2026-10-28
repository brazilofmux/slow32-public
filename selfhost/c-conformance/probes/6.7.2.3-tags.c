// 6.7.2.3p7-11  forward tag declaration, self-reference, `struct t;` in a block starts a new type
#include <stdio.h>
struct node;
struct list { struct node *head; };
struct node { int v; struct node *next; };
struct t { int a; };
int main(void) {
    struct node n2 = { 2, 0 }, n1 = { 1, &n2 };
    struct list l = { &n1 };
    int s = 0; for (struct node *p = l.head; p; p = p->next) s += p->v;
    printf("%d\n", s);
    struct t outer = { 5 };
    { struct t; struct t { char only; } x = { 'q' }; printf("%c %d\n", x.only, outer.a); }
    return 0;
}
