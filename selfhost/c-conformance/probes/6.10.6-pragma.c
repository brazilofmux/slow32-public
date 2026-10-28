// 6.10.6p1  an unrecognized pragma is ignored; pragmas inside a function body
#include <stdio.h>
#pragma probe_unknown_pragma with some tokens (1, 2)
#pragma
int main(void) {
    int s = 0;
#pragma another_unknown_one
    for (int i = 0; i < 4; i++) s += i;
    printf("%d\n", s);
    return 0;
}
