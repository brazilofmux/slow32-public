// 6.10.1p1  defined must be followed by an identifier
// expect: reject
#if defined
#endif
int main(void) { return 0; }
