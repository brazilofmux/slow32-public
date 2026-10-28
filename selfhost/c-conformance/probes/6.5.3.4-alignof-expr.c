// 6.5.3.4p1  _Alignof takes a type name, not an expression
// expect: reject
int main(void) { int x = 0; return (int)_Alignof(x); }
