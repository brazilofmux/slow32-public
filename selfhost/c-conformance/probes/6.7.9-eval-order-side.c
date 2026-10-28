// 6.7.9p23  init lists with side effects; each element initialized (all side effects happen)
#include <stdio.h>
int main(void) {
    int i = 0;
    int a[3] = { i += 1, i += 10, i += 100 };
    printf("%d %d\n", i, a[0] + a[1] + a[2] > 0);
    return 0;
}
