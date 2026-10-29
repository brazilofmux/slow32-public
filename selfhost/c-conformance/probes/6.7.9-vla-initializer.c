// 6.7.9p3  a variable length array may not be initialized
// expect: reject
int f(int n) { int a[n] = { 1 }; return a[0]; }
int main(void) { return f(2); }
