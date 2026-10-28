// 6.7.6.3p7  qualifiers and static inside an array parameter's brackets
#include <stdio.h>
static int sum(int n, int a[restrict static 1]) { int s = 0; for (int i = 0; i < n; i++) s += a[i]; return s; }
static int first(int a[const]) { return a[0]; }
int main(void) { int x[3] = { 1, 2, 3 }; printf("%d %d\n", sum(3, x), first(x)); return 0; }
