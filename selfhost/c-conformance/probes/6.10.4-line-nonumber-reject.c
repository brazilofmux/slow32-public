// 6.10.4p5  after replacement #line must start with a digit sequence
// expect: reject
#line "only-a-name.c"
int main(void) { return 0; }
