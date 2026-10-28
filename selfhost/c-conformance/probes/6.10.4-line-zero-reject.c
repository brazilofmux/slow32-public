// 6.10.4p3  the #line number may not be zero
// expect: reject
#line 0
int main(void) { return 0; }
