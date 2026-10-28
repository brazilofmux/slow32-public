// 6.6p9  address constants: &array[i], array names, function designators in static initializers
#include <stdio.h>
static int garr[6] = {0, 1, 2, 3, 4, 5};
static int f(void) { return 42; }
static int *p1 = &garr[3];
static int (*fp)(void) = f;
static void *tab[] = { garr, (void *)f, &garr[1] };
int main(void) { printf("%d %d %d\n", *p1, fp(), *(int *)tab[2]); return 0; }
