// 6.2.1p7  a tag's scope starts right after it appears; inner tag hides outer
#include <stdio.h>
struct s { int a; };
int main(void) {
    struct node { struct node *next; int v; } n1 = { 0, 7 }, n2 = { &n1, 3 };
    int outer = (int)sizeof(struct s);
    {
        struct s { char c[12]; };
        printf("%d %d %d\n", outer, (int)sizeof(struct s), n2.next->v);
    }
    return 0;
}
