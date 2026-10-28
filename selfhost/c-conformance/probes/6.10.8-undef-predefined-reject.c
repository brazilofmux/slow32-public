// 6.10.8p2  a predefined macro may not be #undef'd
// expect: reject
#undef __LINE__
int main(void) { return 0; }
