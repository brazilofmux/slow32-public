// 6.10.1p1,6.6p3  no assignment operator in a #if expression
// expect: reject
#if 1 = 1
#endif
int main(void) { return 0; }
