// 6.7.9p6  an array designator index must be within the array bounds
// expect: reject
int a[3] = { [5] = 1 };
int main(void) { return a[0]; }
