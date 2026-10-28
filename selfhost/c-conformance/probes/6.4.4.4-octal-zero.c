// 6.4.4.4p5  an octal escape is up to three octal digits even when the first is 0
#include <stdio.h>
int main(void) {
    printf("%d %d %d %d\n", '\012', '\001', '\077', '\00');
    return 0;
}
