// 6.5.2.5p3-6  compound literals are lvalues; arrays, structs, designators; scalar
#include <stdio.h>
struct P { int x, y; };
static int sum(const int *a, int n) { int s = 0; while (n--) s += a[n]; return s; }
int main(void) {
    int *p = (int[]){1, 2, 3, 4};
    struct P q = (struct P){ .y = 9, .x = 8 };
    (struct P){1, 2}.x = 5;              /* modifiable lvalue */
    int s = sum((int[]){10, 20, 30}, 3);
    int *ip = &(int){7};
    *ip += 1;
    printf("%d %d %d %d %d %d\n", p[3], q.x, q.y, s, *ip, (int)sizeof((char[]){"abc"}));
    return 0;
}
