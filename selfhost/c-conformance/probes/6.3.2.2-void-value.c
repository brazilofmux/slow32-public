// 6.3.2.2p1  the nonexistent value of a void expression shall not be used
// expect: reject
void f(void) {}
int main(void) { int x = f(); return x; }
