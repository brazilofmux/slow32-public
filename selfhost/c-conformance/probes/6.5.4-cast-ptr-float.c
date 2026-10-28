// 6.5.4p4  a pointer may not be cast to a floating type
// expect: reject
int main(void) { int x; double d = (double)&x; return d != 0; }
