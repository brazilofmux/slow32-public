// 6.10p1  syntax: every if-section ends with #endif
// expect: reject
#if 1
int main(void) { return 0; }
