// 6.10.4p5  #line operands may come from macro replacement
#include <stdio.h>
#define LNUM 1234
#define LNAME "via-macro.c"
int main(void) {
#line LNUM LNAME
    printf("%d %s\n", __LINE__, __FILE__);
    return 0;
}
