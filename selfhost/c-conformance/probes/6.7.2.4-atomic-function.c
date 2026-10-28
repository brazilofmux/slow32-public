// 6.7.2.4p3  _Atomic(T) may not name a function type
// expect: reject
typedef int F(void);
_Atomic(F) *p;
int main(void) { return 0; }
