// 6.5.16.2p1  += with a pointer left operand needs an integer right operand
// expect: reject
int main(void) { int a[2]; int *p = a; p += 1.5; return 0; }
