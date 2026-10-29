// 6.7.9p11/6.5.16.1p1  an element of a static array of pointers initialized with a nonzero integer
// expect: reject
int g;
int *a[2] = { &g, 5 };
int main(void) { return a[0] != 0; }
