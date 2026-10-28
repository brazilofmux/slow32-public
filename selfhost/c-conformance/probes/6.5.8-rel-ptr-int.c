// 6.5.8p2  a pointer may not be ordered against an integer
// expect: reject
int main(void) { int x; int *p = &x; return p < 5; }
