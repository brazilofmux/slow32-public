// 6.3.2.1p1  an array is not a modifiable lvalue: cannot be assigned
// expect: reject
int main(void) { int a[2] = { 0 }, b[2] = { 1 }; a = b; return a[0]; }
