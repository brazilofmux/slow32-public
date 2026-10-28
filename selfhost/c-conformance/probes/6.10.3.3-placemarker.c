// 6.10.3.3p2,p3  empty arguments next to ## become placemarkers and vanish
#include <stdio.h>
#define T(x, y, z) x ## y ## z
#define S(x) #x
#define XS(x) S(x)
int main(void) {
    int abc = 1, ab = 2, ac = 3, bc = 4, a = 5, b = 6, c = 7;
    int t[] = { T(a,b,c), T(a,b,), T(a,,c), T(,b,c), T(a,,), T(,b,), T(,,c) };
    printf("%d %d %d %d %d %d %d [%s]\n", t[0], t[1], t[2], t[3], t[4], t[5], t[6], XS(T(,,)));
    return 0;
}
