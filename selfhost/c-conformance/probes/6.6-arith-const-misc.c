// 6.6p8  arithmetic constant initializers: sizeof ratios, narrowing casts, negative to unsigned
#include <stdio.h>
static int arr[10];
static int n = (int)(sizeof arr / sizeof *arr);
static char ch = (char)(256 + 'A');
static unsigned u = -1;
static int neg = -7 / 2;
int main(void) { printf("%d %c %u %d\n", n, ch, u, neg); return 0; }
