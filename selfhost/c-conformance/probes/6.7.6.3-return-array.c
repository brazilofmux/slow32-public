// 6.7.6.3p1  a function may not return an array type
// expect: reject
typedef int A[2];
A f(void);
int main(void) { return 0; }
