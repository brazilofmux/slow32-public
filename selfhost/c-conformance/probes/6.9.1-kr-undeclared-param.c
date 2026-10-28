// 6.9.1p6  every identifier in an identifier list shall be declared (no implicit int)
// expect: reject
int f(a, b) int a; { return a + b; }
int main(void) { return f(1, 2); }
