// 6.5.2.4p2  postfix ++/-- on a double changes the object and yields the old value
#include <stdio.h>
int main(void) {
    double d = 1.5; double od = d--;
    double e = 1.5; e++;
    float f = 1.5f; f--;
    printf("%.1f %.1f %.1f %.1f\n", od, d, e, f);
    return 0;
}
