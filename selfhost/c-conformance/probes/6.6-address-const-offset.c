// 6.6p9  an address constant plus or minus an integer constant: arr + 4, &arr[5] - 2, "lit" + 1
#include <stdio.h>
static int garr[6] = {0, 1, 2, 3, 4, 5};
static int *p2 = garr + 4;
static int *p4 = &garr[5] - 2;
static const char *s = "hello" + 1;
int main(void) { printf("%d %d %s\n", *p2, *p4, s); return 0; }
