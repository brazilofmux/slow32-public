// 6.10.3.1p2,6.10.3.2p2  #__VA_ARGS__ spells the merged variable arguments
#include <stdio.h>
#define SHOW(...) #__VA_ARGS__
#define TAIL(a, ...) #__VA_ARGS__
int main(void) {
    printf("<%s> <%s> <%s> <%s>\n", SHOW(x,y), SHOW( p ,  q , r ), TAIL(1, 2, 3), SHOW(x,));
    return 0;
}
