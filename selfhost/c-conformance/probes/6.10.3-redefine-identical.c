// 6.10.3p1,p2  redefinition with an identical list (whitespace amount may differ) is valid
#include <stdio.h>
#define OBJ (1 + 2)
#define OBJ (1   +   2)   /* same tokens, same separations */
#define FN(a) a /* c */ * 2
#define FN(a) a * 2
#define FN(a) a     *     2
int main(void) {
    printf("%d %d\n", OBJ, FN(4));
    return 0;
}
