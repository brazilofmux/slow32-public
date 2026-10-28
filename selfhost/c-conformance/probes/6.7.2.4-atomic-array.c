// 6.7.2.4p3  _Atomic(T) may not name an array type
// expect: reject
typedef int A[3];
_Atomic(A) x;
int main(void) { return 0; }
