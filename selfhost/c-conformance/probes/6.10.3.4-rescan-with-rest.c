// 6.10.3.4p1  rescanning sees the rest of the source: a name produced by expansion
// may take its ( from the text that follows
#include <stdio.h>
#define ADD1(x) ((x) + 1)
#define PICK ADD1
#define APPLY(m) m
int main(void) {
    printf("%d %d\n", PICK(4), APPLY(ADD1)(9));
    return 0;
}
