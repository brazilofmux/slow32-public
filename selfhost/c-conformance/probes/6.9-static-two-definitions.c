// 6.9p3  no more than one external definition of an internal-linkage identifier
// expect: reject
static int x = 1;
static int x = 2;
int main(void) { return x; }
