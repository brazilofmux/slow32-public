// 6.5.7p2  operands of << must have integer type
// expect: reject
int main(void) { double d = 1.0; return (int)(d << 2); }
