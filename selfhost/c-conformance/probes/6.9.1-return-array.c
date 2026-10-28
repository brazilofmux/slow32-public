// 6.9.1p3  a function shall not return an array type
// expect: reject
typedef int A[3];
A f(void) { static A a; return a; }
int main(void) { return 0; }
