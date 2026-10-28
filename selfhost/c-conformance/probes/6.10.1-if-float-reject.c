// 6.10.1p1  #if expression must be an integer constant expression (no floating constant)
// expect: reject
#if 1.5 > 1
#endif
int main(void) { return 0; }
