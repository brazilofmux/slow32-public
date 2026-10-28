// 6.7.9p3  a variable length array may not be initialized
// expect: reject
int main(void) { int n = 3; int a[n] = { 1 }; return a[0]; }
