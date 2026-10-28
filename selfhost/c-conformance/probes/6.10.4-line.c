// 6.10.4p3,p4  #line sets the presumed line number and, optionally, file name
#include <stdio.h>
#include <string.h>
int main(void) {
    int a, b, c;
#line 500
    a = __LINE__;
    b = __LINE__;
#line 77 "renamed.c"
    c = __LINE__;
    printf("%d %d %d %s\n", a, b, c, __FILE__);
    return 0;
}
