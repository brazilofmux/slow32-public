// 6.3.1.8p1  the common real type is the result type: sizeof a double-valued expression is sizeof(double)
#include <stdio.h>
int main(void) {
    double d = 1.0; float f = 1.0f; long long q = 1;
    printf("%d %d %d %d %d\n", (int)sizeof(d + d), (int)sizeof(d + 0), (int)sizeof(f + 1.0),
           (int)sizeof(q + 0), (int)sizeof(f * 2));
    return 0;
}
