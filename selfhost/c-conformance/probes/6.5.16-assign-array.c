// 6.5.16p2  an array is not a modifiable lvalue
// expect: reject
int main(void) { int a[2] = {0}, b[2] = {0}; a = b; return a[0]; }
