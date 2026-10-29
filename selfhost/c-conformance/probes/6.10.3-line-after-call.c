// 6.10.3p10/6.10.8.1  a macro call spanning lines keeps the lines after it: newlines in arguments are white space
#include <stdio.h>
#define TWICE(a, b) ((b) + (b) + (a))
#define DROP(x)
#define STR(x) #x
int main(void) {
    int a, b, c, n;
    n = TWICE(1,
              2);
    a = __LINE__;
    DROP(n
         +
         1);
    b = __LINE__;
    c = TWICE(
        TWICE(1,
              2),
        3) + __LINE__;
    printf("%d %d %d %d %s\n", n, a, b, c, STR(x
                                               y));
    return 0;
}
