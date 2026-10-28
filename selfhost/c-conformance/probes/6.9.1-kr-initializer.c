// 6.9.1p6  declarations in the declaration list shall have no initializer
// expect: reject
int f(a) int a = 3; { return a; }
int main(void) { return f(0); }
