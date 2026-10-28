// 6.5.5p2  operands of % must have integer type
// expect: reject
int main(void) { double d = 5.0; return (int)(d % 2); }
