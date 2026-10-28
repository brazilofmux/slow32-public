// 6.5.6p3  subtracting pointers requires compatible pointed-to types
// expect: reject
int main(void) { int a[2]; double b[2]; return (int)(&a[1] - &b[0]); }
