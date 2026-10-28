// 6.7.3p3  _Atomic qualifier may not be applied to an array type via typedef
// expect: reject
typedef int A[2];
_Atomic A x;
int main(void) { return 0; }
