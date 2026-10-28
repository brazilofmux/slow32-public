// 6.7.9p4/6.6p9  static initializers with address constants: &x, &a[i], linked structs
#include <stdio.h>
static int arr[5] = { 1, 2, 3, 4, 5 };
static int *p = &arr[3];
struct node { int v; struct node *next; };
static struct node n2 = { 2, 0 }, n1 = { 1, &n2 };
int main(void) { printf("%d %d\n", *p, n1.next->v); return 0; }
