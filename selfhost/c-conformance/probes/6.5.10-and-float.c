// 6.5.10p2  operands of binary & must have integer type
// expect: reject
int main(void) { double d = 1.0; return (int)(d & 1); }
