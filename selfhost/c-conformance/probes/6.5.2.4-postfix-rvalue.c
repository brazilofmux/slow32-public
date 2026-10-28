// 6.5.2.4p1  operand of postfix ++ must be a modifiable lvalue
// expect: reject
int main(void) { const int c = 1; c++; return 0; }
