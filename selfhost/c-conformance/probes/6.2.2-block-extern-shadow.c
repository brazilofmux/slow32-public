// 6.2.2p4, 6.2.1p4  block-scope extern names the file-scope object even while a local of that name is visible outside the block
#include <stdio.h>
int x = 1;
int main(void) {
    int x = 2;
    int r;
    { extern int x; r = x; }
    printf("%d %d\n", r, x);
    return 0;
}
