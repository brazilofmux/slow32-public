// 6.7.4p4  main may not be declared inline (hosted)
// expect: reject
inline int main(void) { return 0; }
