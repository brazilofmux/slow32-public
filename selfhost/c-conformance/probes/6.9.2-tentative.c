// 6.9.2p2  repeated tentative definitions merge; with no initializer anywhere the object is zero
#include <stdio.h>
int a;
int a;
int b;
int b = 5;
int b;
static int s;
static int s = 9;
static int z;
struct pt { int x, y; };
struct pt pv;
struct pt pv;
double dz;
int *pz;
int main(void) {
    printf("%d %d %d %d %d %d %d\n", a, b, s, z, pv.x + pv.y, dz == 0.0, pz == 0);
    return 0;
}
