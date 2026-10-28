// 6.10.1p1  #if needs a constant expression; an empty one is invalid
// expect: reject
#if
#endif
int main(void) { return 0; }
