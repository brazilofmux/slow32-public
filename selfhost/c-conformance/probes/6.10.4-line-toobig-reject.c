// 6.10.4p3  the #line number may not exceed 2147483647
// expect: reject
#line 2147483648
int main(void) { return 0; }
