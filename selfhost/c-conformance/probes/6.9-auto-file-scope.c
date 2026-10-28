// 6.9p2  auto shall not appear in an external declaration
// expect: reject
auto int x;
int main(void) { return 0; }
