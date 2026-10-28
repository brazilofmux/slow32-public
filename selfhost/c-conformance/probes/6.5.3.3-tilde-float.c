// 6.5.3.3p1  the operand of ~ must have integer type
// expect: reject
int main(void) { double d = 1.0; return (int)~d; }
