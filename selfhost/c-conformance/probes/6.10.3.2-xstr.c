// 6.10.3.1p1,6.10.3.2p2  #x suppresses expansion; one level of indirection expands first
#include <stdio.h>
#define VAL 42
#define STR(x) #x
#define XSTR(x) STR(x)
int main(void) {
    printf("%s %s %s\n", STR(VAL), XSTR(VAL), XSTR(__LINE__));
    return 0;
}
