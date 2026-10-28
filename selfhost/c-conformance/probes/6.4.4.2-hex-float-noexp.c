// 6.4.4.2p1  a hexadecimal floating constant requires the binary exponent part
// expect: reject
int main(void) { double d = 0x1.8; return (int)d; }
